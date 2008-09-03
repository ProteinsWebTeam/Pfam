
# BatchSearch.pm
# jt6 20061108 WTSI
#
# $Id: BatchSearch.pm,v 1.1 2008-09-03 15:40:43 jt6 Exp $

=head1 NAME

PfamBase::Controller::Search::BatchSearch - parent class for batch searches

=cut

package PfamBase::Controller::Search::BatchSearch;

=head1 DESCRIPTION

This is the parent class for batch search operations.

$Id: BatchSearch.pm,v 1.1 2008-09-03 15:40:43 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamBase::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 check_unique : Private

Queries the web_user database to check if the current sequence has been 
submitted by this user, with the same search options, within the last "n" hours,
where n is set in the configuration. If this is a duplicate search, we 
return 1, otherwise 0;

=cut

sub check_unique : Private {
  my ( $this, $c ) = @_;

  my $n = $this->{uniqueSearchInterval};

  my $rs = $c->model( 'WebUser::JobHistory' )
           ->search( { options => $c->stash->{options},
                       email   => $c->stash->{email},
                       stdin   => $c->stash->{input},
                       opened  => \"> DATE_SUB( NOW(), INTERVAL $n HOUR )" },
                     { join => [ 'job_stream' ] } );

  if ( $rs->count() > 0 ) {
    $c->stash->{searchError} = 
      "You have submitted exactly this search within the last $n hours. Please try not to submit duplicate searches.";

    $c->log->debug( 'Search::BatchSearch::check_unique: found ' . $rs->count() 
                    . " identical searches within the last $n hours" )
      if $c->debug;

    return 0;
  }
  
  return 1;
}

#-------------------------------------------------------------------------------

=head2 parse_upload : Private

Parses the uploaded file and, if it's valid, copies it to the stash. Returns 1
if the upload passed validation, 0 otherwise. Error messages explaining the
failure will be returned in the stash, with the key "errorMsg".

=cut

sub parse_upload : Private {
  my ( $this, $c ) = @_;

  # check that we can get an Catalyst::Request::Upload object from the request 
  my $u;
  unless ( $u = $c->req->upload('batchSeq') ) {
    $c->stash->{errorMsg} = 
      'You did not supply a valid FASTA format file. Please try again.';

    $c->log->warn( 'Search::BatchSearch::parse_upload: no "batchSeq" parameter found or content empty' )
      if $c->debug;

    return 0;
  }
  
  # check that the Upload object returns us a filehandle
  my $fh;
  unless ( $fh = $u->fh ) {
    $c->stash->{errorMsg} =
        'There was a problem with your file upload. Please try again. If you '
      . 'continue to have problems uploading, please report this to the helpdesk.';

    $c->log->warn( 'Search::BatchSearch::parse_upload: couldn\'t open uploaded file' )
      if $c->debug;

    return 0;
  }

  # read through the file and bail if we find any illegal characters in it  
  my $seq;                # sequence of each FASTA "block"
  my $line_num = 0;       # number of lines in the file
  my $seq_count = 0;      # number of sequences in the file
  my $seq_line_count = 0; # number of lines in a FASTA "block"
  my $header = '';        # the current header line
  my %header_lines;       # line numbers for sequence headers
  my %sequences;          # the sequence strings
  
  while ( <$fh> ) {
    
    # keep track of the number of lines in the file. Increment before ignoring
    # blank lines though, so that we get the right line count for error
    # messages
    $line_num++;

    # check we're not exceeding the maximum total number of lines
    if ( $line_num > $this->{maxNumLines} ) {
      $c->stash->{errorMsg} = 
          'Your sequence file is too long. The server currently allows a maximum of '
        . $this->{maxNumLines} . ' in a single upload. Please split your sequences '
        . 'across several shorter files and submit them individually.'; 

      $c->log->debug( 'Search::BatchSearch::parse_upload: file is too long (>'
                      . $this->{maxNumLines} . ')' ) if $c->debug;

      return 0;
    }

    #----------------------------------------

    # illegal character checks
    
    # look at header lines
    if ( m/^>(.*)/ ) {

      $header                = $1;
      $header_lines{$header} = $line_num;

      # check header lines. We're banning the following characters: ; \ ! and *
      if ( m/\;\\\!\*/ ) {
        $c->stash->{errorMsg} = 
            "We found an illegal character in the header on line $line_num " 
          . 'of your input file. Please check the file format and try again.';

        $c->log->debug( 'Search::BatchSearch::parse_upload: illegal character in header; bailing' )
          if $c->debug;

        return 0;
      }

      # total number of sequences
      if ( $seq_count++ > $this->{maxNumSeqs} ) {
        $c->stash->{errorMsg} = 
            'There are too many sequences in your file. The server currently '
          . 'allows a maximum of ' . $this->{maxNumSeqs} . ' in a single file. '
          . 'Please split your sequences into multiple files and submit each one '
          . 'individually';

        $c->log->debug( "Search::BatchSearch::parse_upload: too many sequences ($seq_count > "
                        . $this->{maxNumSeqs} . ')' ) if $c->debug;

        return 0;
      }

      # strip both new line and carriage return
      s/[\r\n]//g;
    }
    
    # look at sequence lines
    else {

      # regular sequence line (no "J" or "O" allowed)
      unless ( m/^[ABCDEFGHIKLMNPQRSTUVWXYZ\-\*\s]+\r?$/i ) {
        $c->stash->{errorMsg} = 
            "We found an illegal character in the sequence on line $line_num "
          . 'of your input file. Please check the file format and try again.';

        $c->log->debug( 'Search::BatchSearch::parse_upload: illegal character in sequence; bailing' )
          if $c->debug;

        return 0;
      }

      # it's a valid sequence line; store the cleaned-up contents of the line,
      # so that we can perform more checks later
      $sequences{$header} .= $_;
      
      # strip new line, carriage return and *space characters*
      s/[\r\n\s]//g;

    }
    
    # don't bother storing blank lines in the DB
    next if m/^\s*$/;
    
    # this line was valid; add it to the input, along with a newline. This is
    # actual input that will be put into the database
    $c->stash->{input} .= $_ . "\n";

  } # end of "while"

  $c->log->debug( "Search::BatchSearch::parse_upload: found |$seq_count| sequences in the upload" )
    if $c->debug;

  #----------------------------------------

  # check each of the sequences in more detail

  foreach my $header ( sort keys %sequences ) {
    my $seq = $sequences{$header};

    # make sure that this sequence isn't too long
    $c->log->debug( 'Search::BatchSearch::parse_upload: found |' . length( $seq )
                    . '| residues in sequence' ) if $c->debug;

    if ( length $seq > $this->{maxNumResidues} ) {
      $c->stash->{errorMsg} =
          "The sequence starting on line $header_lines{$header} is too long. "
        . 'The server currently allows a maximum of ' . $this->{maxNumResidues} 
        . ' residues per sequence. Please make sure that your sequences are shorter '
        . 'than ' . $this->{maxNumResidues} . ' residues and try again.';

      $c->log->debug( 'Search::BatchSearch::parse_upload: too many residues in sequence (>' 
                      . $this->{maxNumResidues} . '); bailing' ) if $c->debug;

      return 0;
    }

    # check the composition of the sequence. Calculate the frequency with which
    # each residue type appears
    my %frequency;
    foreach ( split '', $seq ) {
      next unless m/[A-Z]/i;
      $frequency{$_}++;
    }
    
    # check that we have a reasonable number of different residue types in the 
    # sequence. If there are too few residue types, flag the sequence as 
    # suspicious
    my $num_residue_types = scalar( keys %frequency );
    $c->log->debug( "Search::BatchSearch::parse_upload: found |$num_residue_types| "
                    . 'different residue types' ) if $c->debug;
                    
    if ( $num_residue_types < $this->{minNumResidueTypes} ) {
      $c->stash->{errorMsg} =
          qq(The sequence starting at line '$header_lines{$header}' does not look )
        .  q(like a protein sequence. Please check your sequence and try again. If )
        .  q(you feel that there is a problem with the server, please contact the )
        .  q(Pfam helpdesk at the address below and we will be happy to take a look.);

      $c->log->debug( 'Search::BatchSearch::parse_upload: not enough sequence variation; found '
        . "$num_residue_types different residue types (<" 
        . $this->{minNumResidueTypes} . '); bailing' ) if $c->debug;
      
      return 0;
    }

  }

  # the upload validated; return true
  return 1;
}

#-------------------------------------------------------------------------------

=head2 check_sequence : Private

A bit of sequence analysis...

=cut

# TODO: NOT USED CURRENTLY

sub check_sequence : Private {
  my( $this, $c, $seq ) = @_;

  my %frequency;
  foreach ( split '', $seq ) {
    next unless m/[A-Z]/i;
    $frequency{$_}++;
  }
  
  # check we have a reasonable number of different residue types in the 
  # sequence
  my $N = scalar( keys %frequency );
  $c->log->debug( "Search::Batch::check_sequence: found |$N| different residue types" )
    if $c->debug;
  if( $N < $this->{minNumResidueTypes} ) {
    $c->log->debug( 'Search::Batch::check_sequence: not enough sequence variation; found '
      . $N . ' different residue types (<' 
      . $this->{minNumResidueTypes} . ' residue types); bailing' ) if $c->debug;
    return;
  }
  
  # check we have a sensible distribution of residues

  # for each residue type found in a sequence, we record the number of 
  # occurrences. We then calculate the mean number of occurrences for all 
  # residue types, and the standard deviation for each type from that mean.
  # If the occurrence count for a residue type is greater than one standard
  # deviation from the mean, we flag it

  # calculate the mean and standard deviation for the residue counts
  my $mean;
  foreach ( keys %frequency ) {
    my $value = $frequency{$_};
    $c->log->debug( "Search::Batch::check_sequence: residue type |$_|, occurrences: |$value|" )
      if $c->debug;
    $mean += $value;
  }
  $mean = $mean / $N;
  $c->log->debug( "Search::Batch::check_sequence: mean number of occurrences: |$mean|" )
    if $c->debug;

  my $sd = 0;
  foreach ( values %frequency ) {
    $sd += ( $_ - $mean ) ** 2;
  }
  $sd = sqrt( $sd / ( $N - 1 ) );
  $c->log->debug( "Search::Batch::check_sequence: standard deviation: |$sd|" )
    if $c->debug;
  
  my $low_deviations = 0;
  foreach ( keys %frequency ) {
    my $diff = abs( $frequency{$_} - $mean );
    if( $diff < 0.5 * $sd ) {
      $c->log->debug( "Search::Batch::check_sequence: diff for |$_| (abs($frequency{$_}-$mean)=$diff) < 0.5*sd; flagging" )
        if $c->debug;
      $low_deviations++;
    }
  }
  
  if( $low_deviations > 10 ) {
    $c->log->debug( "Search::Batch::check_sequence: too many low deviations ($low_deviations > 10" )
      if $c->debug;
    return;
  }

  return 1;
}

# Ben's solution to the problem of finding repeats in sequences
#
##!/usr/bin/env perl
#use warnings;
#use strict;
#use IO::File;
#
#my $seq;
#my $in_f = shift;
#if ($in_f)
#{
#    my $fh = IO::File->new($in_f, 'r') or die "Error reading $in_f: $!";
#    while (<$fh>)
#    {
#        chomp;
#        $seq .= $_ unless /^>/;
#    }
#}
#else
#{
#    while (<STDIN>)
#    {
#        chomp;
#        $seq .= $_ unless /^>/;
#    }
#}
#
#die "Usage: $0 [infile]\n" unless $seq;
#
#my $len = length($seq);
#my @factors = split /\s+/, `factor $len`;
#shift @factors;
#
#if (@factors == 1)
#{
#    exit;
#}
#
#my $factor = 1;
#foreach (sort sort {$a <=> $b} @factors)
#{
#    push(@factors, ($factor * $_));
#    $factor *= $_;
#}
#
#@factors = sort {$a <=> $b} @factors;
#
#my %seen;
#foreach (@factors)
#{
#    next if $seen{$_}++;
#    print STDERR "Factor $_\n";
#    my $num = $len/$_;
#    print STDERR "Num $num\n";
#    my $pattern = unpack "a$_", $seq;
#    if ($seq eq ($pattern x $num))
#    {
#        print "Found a repeated pattern of length $_ repeated $num times!\n";
#    }
#}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
