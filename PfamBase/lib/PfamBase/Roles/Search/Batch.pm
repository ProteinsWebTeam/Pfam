
# Batch.pm
# jt6 20120515 WTSI
#
# $Id$

=head1 NAME

PfamBase::Roles::Search::BatchSearch - role to add some utility methods for 
batch search controllers

=cut

package PfamBase::Roles::Search::Batch;

=head1 DESCRIPTION

This role provides some common actions for the batch search controllers.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

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
failure will be returned in the stash, with the key "searchError".

=cut

sub parse_upload : Private {
  my ( $this, $c ) = @_;

  # check that we can get an Catalyst::Request::Upload object from the request 
  my $u;
  unless ( $u = $c->req->upload('batchSeq') ) {
    $c->stash->{searchError} = 
      'You did not supply a valid FASTA format file. Please try again.';

    $c->log->warn( 'Search::BatchSearch::parse_upload: no "batchSeq" parameter found or content empty' )
      if $c->debug;

    return 0;
  }
  
  # check that the Upload object returns us a filehandle
  my $fh;
  unless ( $fh = $u->fh ) {
    $c->stash->{searchError} =
        'There was a problem with your file upload. Please try again. If you '
      . 'continue to have problems uploading, please report this to the helpdesk.';

    $c->log->warn( 'Search::BatchSearch::parse_upload: couldn\'t open uploaded file' )
      if $c->debug;

    return 0;
  }

  my $line_num       = 0;  # number of lines in the file
  my $seq_count      = 0;  # number of sequences in the file
  my $seq_line_count = 0;  # number of lines in a FASTA "block"
  my $header         = ''; # the current header line
  my %header_lines;        # line numbers for sequence headers
  my %short_header_lines;  # header lines shortened to 60 characters
  my %sequences;           # the sequence strings
  
  while ( <$fh> ) {
    
    # keep track of the number of lines in the file. Increment before ignoring
    # blank lines though, so that we get the right line count for error
    # messages
    $line_num++;

    # don't bother storing blank lines in the DB
    next if m/^\s*$/;
    
    #----------------------------------------

    # check we're not exceeding the maximum total number of lines
    if ( $line_num > $this->{maxNumLines} ) {
      $c->stash->{searchError} = 
          'Your sequence file is too long. The server currently allows a maximum of '
        . $this->{maxNumLines} . ' in a single upload. Please split your sequences '
        . 'across several shorter files and submit them individually.'; 

      $c->log->debug( 'Search::BatchSearch::parse_upload: file is too long (>'
                      . $this->{maxNumLines} . ')' ) if $c->debug;

      return 0;
    }

    #----------------------------------------

    # look at header lines
    if ( m/^>(\S+)/ ) {
      # my $raw_header = $1;
      $header = $1;

      # check that the header text doesn't start with a space...
      if ( $header =~ m/^\s+/ ) {
        $c->stash->{searchError} = 
            "You cannot have whitespace after the '>'. Please check that your file "
          . "conforms to the FASTA file format specification.";

        $c->log->debug( 'Search::BatchSearch::parse_upload: whitespace after ">"' )
          if $c->debug;

        return 0;
      }

      # check that the header has some content... Yes, we've had a user 
      # submit sequence files with empty header lines.
      unless ( length $header ) {
        $c->stash->{searchError} = 
            'You cannot have blank header lines. Please make sure that any line '
          . "starting with '>' has content.";

        $c->log->debug( 'Search::BatchSearch::parse_upload: empty header line' )
          if $c->debug;

        return 0;
      }

      # store the header line here unchecked; we'll validate it in a moment.
      #
      # we work with a shortened header line because hmmpfam only considers the 
      # first 63 characters and we need to hash on the string that hmmpfam
      # provides when we come to look at the output in the active site code in 
      # pfam_scan.pl

      # if ( length $raw_header > 60 ) {
      #   $header = substr $raw_header, 0, 60;
      # }
      # else {
      #   $header = $raw_header;
      # } 

      # check for the following illegal characters: \ ! and *
      if ( m/[\\\!\*]/ ) {
        $c->stash->{searchError} = 
            "We found an illegal character in the header on line $line_num " 
          . 'of your input file. Please check the file format and try again.';

        $c->log->debug( 'Search::BatchSearch::parse_upload: illegal character in header; bailing' )
          if $c->debug;

        return 0;
      }

      # check that we haven't already seen this (possibly truncated) header line
      if ( defined $header_lines{$header} ) {
        $c->stash->{searchError} = 
            'Your file appears to contain duplicate sequences. The header on '
          . "line $line_num was also found on line $header_lines{$header}. "
          . 'Please make sure that your file contains only unique header lines. '
          . 'See the notes for more information about this restriction.';

        $c->log->debug( "Search::BatchSearch::parse_upload: duplicate header on line $line_num" )
          if $c->debug;

        return 0;
      }

      # not a duplicate header, so store it
      $header_lines{$header} = $line_num;

      # check that the total number of sequences doesn't exceed some limit
      if ( $seq_count++ > $this->{maxNumSeqs} ) {
        $c->stash->{searchError} = 
            'There are too many sequences in your file. The server currently '
          . 'allows a maximum of ' . $this->{maxNumSeqs} . ' sequences in a '
          . 'single file. Please split your sequences into multiple files and '
          . 'submit each one individually';

        $c->log->debug( "Search::BatchSearch::parse_upload: too many sequences ($seq_count > "
                        . $this->{maxNumSeqs} . ')' ) if $c->debug;

        return 0;
      }

      # strip both new line and carriage return, leaving spaces alone. We do 
      # this last, rather than first as with sequence lines
      s/[\r\n]//g;
    }
    
    # done checking header lines; look at sequence lines
    else {

      # strip new line, carriage return and *space characters*
      s/[\r\n\s]//g;
    
      # regular sequence line
      my $regex_string = $this->{sequenceValidationRegex};      
      my $regex = qr/$regex_string/i;
      unless ( m/$regex/ ) {
        $c->stash->{searchError} = 
            "We found an illegal character in the sequence on line $line_num "
          . 'of your input file. Please check the file format and try again.';

        $c->log->debug( 'Search::BatchSearch::parse_upload: illegal character in sequence; bailing' )
          if $c->debug;

        return 0;
      }

      # make sure that $header is set. If it's not set, the sequence wasn't in
      # FASTA format and we need to admonish the user...
      unless ( defined $header and $header ne '' ) {
        $c->stash->{searchError} = 
            'Your uploaded file does not appear to be in FASTA format. Please '
          . 'check the file format and try again.';

        $c->log->debug( 'Search::BatchSearch::parse_upload: no FASTA header line found; bailing' )
          if $c->debug;

        return 0;
      }

      # OK. Now we're looking at a valid sequence line; store the cleaned-up 
      # contents of the line, so that we can perform more checks later
      $sequences{$header} .= $_;
    }
    
    # this line was valid; add it to the input, along with a newline. This is
    # actual input that will be put into the database
    $c->stash->{input} .= $_ . "\n";

  } # end of "while"

  $c->log->debug( "Search::BatchSearch::parse_upload: found |$seq_count| sequences in the upload" )
    if $c->debug;

  # sequences will only be stored in the %sequences hash if they have a valid
  # header and a valid sequence string. Check the sequences hash and make sure
  # it's sensible...

  # make sure we actually got some sequences
  unless ( scalar keys %sequences ) {
    $c->stash->{searchError} = 
        'We could not find any valid sequences in the uploaded file. Please '
      . 'check the file format and try again.';

    $c->log->debug( 'Search::BatchSearch::parse_upload: no valid sequences found in file' )
      if $c->debug;

    return 0;
  }

  # make sure we got the same number of headers as sequences
  if ( scalar( keys %sequences ) != $seq_count ) {
    $c->stash->{searchError} = 
        "The file contained $seq_count headers but " . scalar( keys %sequences )
      . ' sequences. Please check the file format and try again.';

    $c->log->debug( 'Search::BatchSearch::parse_upload: different numbers of headers and sequences' )
      if $c->debug;

    return 0;
  }

  #----------------------------------------

  # check each of the sequences in more detail

  foreach $header ( sort keys %sequences ) {
    my $seq = $sequences{$header};

    $c->log->debug( 'Search::BatchSearch::parse_upload: found |' . length( $seq )
                    . '| residues in sequence' ) if $c->debug;

    # make sure that this sequence isn't too long
    if ( length $seq > $this->{maxNumResidues} ) {
      $c->stash->{searchError} =
          "The sequence starting on line $header_lines{$header} is too long. "
        . 'The server currently allows a maximum of ' . $this->{maxNumResidues} 
        . ' characters per sequence. Please make sure that your sequences are shorter '
        . 'than ' . $this->{maxNumResidues} . ' and try again.';

      $c->log->debug( 'Search::BatchSearch::parse_upload: too many characters in sequence (>' 
                      . $this->{maxNumResidues} . '); bailing' ) if $c->debug;

      return 0;
    }

    # nor too short...
    if ( defined $this->{minNumResidues} and length $seq < $this->{minNumResidues} ) {
      $c->stash->{searchError} =
          "The sequence starting on line $header_lines{$header} is too short. "
        . 'The server currently requires sequences to be a minimum of '
        . $this->{minNumResidues} 
        . ' characters in length. Please make sure that your sequences are longer '
        . 'than ' . $this->{minNumResidues} . ' and try again.';

      $c->log->debug( 'Search::BatchSearch::parse_upload: not enough characters in sequence (<' 
                      . $this->{minNumResidues} . '); bailing' ) if $c->debug;

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
      $c->stash->{searchError} =
          qq(The sequence starting at line $header_lines{$header} does not look )
        .  q(like a real sequence. Please check your sequence and try again. If )
        .  q(you feel that there is a problem with the server, please contact the )
        .  q(helpdesk at the address below and we will be happy to take a look.);

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
