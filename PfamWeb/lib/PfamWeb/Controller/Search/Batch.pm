
# Batch.pm
# jt6 20061108 WTSI
#
# $Id: Batch.pm,v 1.10 2008-04-25 10:17:26 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Batch - perform protein sequence batch searches

=cut

package PfamWeb::Controller::Search::Batch;

=head1 DESCRIPTION

This controller is responsible for running batch searches for protein sequences.
It uses the base class L<Batch|PfamWeb::Controller::Search::Batch> to take
care of queuing the search, but the validation of input etc. is here.

$Id: Batch.pm,v 1.10 2008-04-25 10:17:26 jt6 Exp $

=cut

use strict;
use warnings;

use Scalar::Util qw( looks_like_number );
use Email::Valid;

use base 'PfamWeb::Controller::Search::BatchSearch';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 search : Path

Executes a protein sequence batch search. 

=cut

sub search : Path {
  my( $this, $c ) = @_;

  # validate the input
  $c->forward( 'validateInput' );
  if( $c->stash->{searchError} ) {
    $c->stash->{batchSearchError } = $c->stash->{searchError};
    return;
  }

  # build the command to run
  my $opts;
  $opts .=  q( --mode ) . $c->stash->{batchOpts} if( $c->stash->{batchOpts} ne 'both' and 
                                                     $c->stash->{batchOpts} ne 'bothNoMerge' );
  $opts .=  q( --no_merge )                      if( $c->stash->{batchOpts} eq 'bothNoMerge' );
  $opts .=  q( -e )     . $c->stash->{evalue}    if( $c->stash->{evalue} and not $c->stash->{ga} );
  $opts .=  q( --overlap )                       if( $c->stash->{showOverlap} );
  
  $c->stash->{options} = $opts;

  # set the queue
  $c->stash->{job_type} = 'batch';

  # and submit the job...
  $c->forward( 'queueSearch' );
  if( $c->stash->{searchError} ) {
    $c->stash->{batchSearchError } = $c->stash->{searchError};
    return;
  }

  # if we get to here then the job was submitted successfully. Before handing
  # off to the template, set a refresh URI that will be picked up by head.tt 
  # and used in a meta refresh element
  $c->stash->{refreshUri}   = $c->uri_for( '/search' );
  $c->stash->{refreshDelay} = 30;
  
  $c->log->debug( 'Search::Batch::search: protein batch search submitted' )
    if $c->debug; 
  $c->stash->{template} = 'pages/search/sequence/batchSubmitted.tt';
}

#-------------------------------------------------------------------------------
#- private actions--------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 validateInput : Private

Validate the form input. Error messages are returned in the stash as 
"searchError".

=cut

sub validateInput : Private {
  my( $this, $c ) = @_;
  
  # the sequence itself
  unless( $c->forward( 'parseUpload' ) ) {

    $c->stash->{searchError} = 
         $c->stash->{errorMsg}
      || 'No valid sequence file found. Please enter a valid FASTA-format file and try again.';

    $c->log->debug( 'Search::Batch::search: bad FASTA file; returning to form' )
      if $c->debug;
    return;
  }

  # sequence search options
  if( defined $c->req->param( 'batchOpts' ) ) {
    $c->stash->{batchOpts} = $c->req->param( 'batchOpts' );
    unless( $c->stash->{batchOpts} eq 'both' or
            $c->stash->{batchOpts} eq 'bothNoMerge' or
            $c->stash->{batchOpts} eq 'ls' or
            $c->stash->{batchOpts} eq 'fs' ) {
      $c->stash->{searchError} = 'You must select a valid search option.';

      $c->log->debug( 'Search::Batch::search: bad search option; returning to form' )
        if $c->debug;
      return;
    }
  } else {
    $c->log->debug( 'Search::Batch::search: search options not specified; returning to form' )
      if $c->debug;
    $c->stash->{searchError} = 'You must select a search option.';
    return;
  }

  # if we have an evalue, we'll use that, otherwise we'll use the gathering
  # threshold
  if( defined $c->req->param( 'ga' ) and $c->req->param( 'ga' ) ) {
    $c->stash->{ga} = 1;
  } else {
    if( defined $c->req->param( 'evalue' ) and 
        looks_like_number( $c->req->param( 'evalue' ) ) ) {
      $c->stash->{evalue} = $c->req->param( 'evalue' );
    } else {
      $c->stash->{searchError} = 'You did not enter a valid E-value.';

      $c->log->debug( 'Search::Batch::search: bad evalue; returning to form' )
        if $c->debug;
      return;
    }
  }

  # email address
  if( Email::Valid->address( -address => $c->req->param('email') ) ) {
    $c->stash->{email} = $c->req->param('email');
  } else {
    $c->stash->{searchError} = 'You did not enter a valid email address.';

    $c->log->debug( 'Search::Batch::search: bad email address; returning to form' )
      if $c->debug;
    
    return;
  }  

  # passed !
  $c->log->debug( 'Search::Batch::search: input parameters all validated' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 parseUpload : Attribute

Parses the uploaded file and, if it's valid, copies it to the stash.

=cut

sub parseUpload : Private {
  my( $this, $c ) = @_;

  # check that we can get an Catalyst::Request::Upload object from the request 
  my $u;
  unless( $u = $c->req->upload('batchSeq') ) {
    $c->log->warn( 'Search::Batch::parseUpload: no "batchSeq" parameter found or content empty' );
    $c->stash->{errorMsg} =
      'You did not supply a valid FASTA format file. Please try again.';
    return;
  }
  
  # check that the Upload object returns us a filehandle
  my $fh;
  unless( $fh = $u->fh ) {
    $c->log->warn( 'Search::Batch::parseUpload: couldn\'t open uploaded file' );
    $c->stash->{errorMsg} =
        'There was a problem with your file upload. Please try again. If you '
      . 'continue to have problems uploading, please report this to the Pfam helpdesk.';
    return;
  }

  # read through the file and bail if we find any illegal characters in it  
  my $seq;                # sequence of each FASTA "block"
  my $line_num = 0;       # number of lines in the file
  my $seq_count = 0;      # number of sequences in the file
  my $seq_line_count = 0; # number of lines in a FASTA "block"
  my $header = '';        # the current header line
  my %header_lines;       # line numbers for sequence headers
  my %sequences;          # the sequence strings
  
  while( <$fh> ) {
    
    # keep track of the number of lines in the file. Increment before ignoring
    # blank lines though, so that we get the right line count for error
    # messages
    $line_num++;

    # check we're not exceeding the maximum total number of lines
    if( $line_num > $this->{maxNumLines} ) {
      $c->log->debug( 'Search::Batch::parseUpload: file is too long (>'
                      . $this->{maxNumLines} . ')' ) if $c->debug;
      $c->stash->{errorMsg} = 
          'Your sequence file is too long. The server currently allows a maximum of '
        . $this->{maxNumLines} . ' in a single upload. Please split your sequences '
        . 'across several shorter files and submit them individually.'; 
      return;
    }

    # ignore blank lines 
    next if m/^\s*$/;
    
    #----------------------------------------

    # illegal character checks
    if( m/^>(.*)/ ) {

      $header                = $1;
      $header_lines{$header} = $line_num;

      # check header lines. We're banning the following characters: ; \ ! and *
      if( m/\;\\\!\*/ ) {
        $c->log->debug( 'Search::Batch::parseUpload: illegal character in header; bailing' )
          if $c->debug;
        $c->stash->{errorMsg} = 
            "We found an illegal character in the header on line $line_num " 
          . 'of your input file. Please check the file format and try again.';
        return;
      }

      # total number of sequences
      if( $seq_count++ > $this->{maxNumSeqs} ) {
        $c->log->debug( "Search::Batch::parseUpload: too many sequences ($seq_count > "
                        . $this->{maxNumSeqs} . ')' ) if $c->debug;
        $c->stash->{errorMsg} = 
            'There are too many sequences in your file. The server currently '
          . 'allows a maximum of ' . $this->{maxNumSeqs} . ' in a single file. '
          . 'Please split your sequences into multiple files and submit each one '
          . 'individually';
        return;
      }

    } else {

      # regular sequence line (no "J" or "O" allowed)
      unless( m/^[ABCDEFGHIKLMNPQRSTUVWXYZ\-\*\s]+\r?$/i ) {
        $c->log->debug( 'Search::Batch::parseUpload: illegal character in sequence; bailing' )
          if $c->debug;
        $c->stash->{errorMsg} = 
            "We found an illegal character in the sequence on line $line_num "
          . 'of your input file. Please check the file format and try again.';
        return;
      }

      # it's a valid sequence line; store the cleaned-up contents of the line
      chomp;  # chop off newlines
      s|\s||g; # remove spaces and tabs
      $sequences{$header} .= $_;
      
    }

    # this line was valid; add it to the input
    $c->stash->{input} .= $_;

  } # end of "while"

  $c->log->debug( "Search::Batch::parseUpload: found |$seq_count| sequences in the upload" )
    if $c->debug;

  #----------------------------------------

  # check each of the sequences in more detail
  foreach my $header ( sort keys %sequences ) {
    my $seq = $sequences{$header};

    $c->log->debug( 'Search::Batch::parseUpload: checking sequence |' 
                    . $header . '|:' . $seq  ) if $c->debug;
    
    # make sure that this sequence isn't too long
    $c->log->debug( 'Search::Batch::parseUpload: found |' . length( $seq )
                    . '| residues in sequence' ) if $c->debug;

    if( length $seq > $this->{maxNumResidues} ) {
      $c->log->debug( 'Search::Batch::parseUpload: too many residues in sequence (>' 
                      . $this->{maxNumResidues} . '); bailing' ) if $c->debug;
      $c->stash->{errorMsg} =
          "The sequence starting on line $header_lines{$header} is too long. "
        . 'The server currently allows a maximum of ' . $this->{maxNumResidues} 
        . ' residues per sequence. Please make sure that your sequences are shorter '
        . 'than ' . $this->{maxNumResidues} . ' residues and try again.';
      return;
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
    $c->log->debug( "Search::Batch::check_sequence: found |$num_residue_types| "
                    . 'different residue types' ) if $c->debug;
    if( $num_residue_types < $this->{minNumResidueTypes} ) {
      $c->log->debug( 'Search::Batch::check_sequence: not enough sequence variation; found '
        . "$num_residue_types different residue types (<" 
        . $this->{minNumResidueTypes} . '); bailing' ) if $c->debug;
      $c->stash->{errorMsg} = 
          "The sequence starting at line '$header_lines{$header}' does not look "
        . 'like a protein sequence. Please check your sequence and try again. If '
        . 'you feel that there is a problem with the server, please contact the '
        . 'Pfam helpdesk at the address below and we will be happy to take a look.';
      return;
    }

  }

  # the upload validated; return true
  return 1;
}

#-------------------------------------------------------------------------------

=head2 check_sequence : Private

A bit of sequence analysis...

=cut

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

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;
