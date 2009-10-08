# SeqFetch.pm
#
# Author:        finnr
# Maintainer:    $Id: SeqFetch.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $
# Version:       $Revision: 1.1 $
# Created:       Nov 20, 2008
# Last Modified: $Date: 2009-10-08 12:27:28 $

=head1 NAME

SeqFetch - Fetches sequences from a sequence database

=cut

package Bio::Pfam::SeqFetch;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: SeqFetch.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $

=head1 COPYRIGHT

File: SeqFetch.pm

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk)

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

use strict;
use warnings;
use Carp;
use File::Temp qw(tempfile);

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 parseGDF 

  Title    : parseGDF
  Usage    : Bio::Pfam::SeqFetch::parseGDF(\*GDF, \%seqsList)
  Function : Parse a GDF file and added them to a standard data structure for later fetching.
  Args     : A file handle to a GDF file, a reference to a hash
  Returns  : The number of sequence regions added to hashref
  
=cut

sub parseGDF {
  my ( $FH, $seqListRef ) = @_;
  my $noSeqsRequested = 0;
  while (<$FH>) {
    if (/^(\S+)\s+(\d+)\s+(\d+)\s+(\S+)/) {
      push( @{ $$seqListRef{$4} }, { start => $2, end => $3 } );
      $noSeqsRequested++;
    }
    else {
      die "Died not parse line in GDF file: [$_]\n";
    }
  }
  return ($noSeqsRequested);
}

=head2 parseScores 

  Title    : parseScores
  Usage    : Bio::Pfam::SeqFetch::parseScores(\*S, \%seqsList)
  Function : Parse a scores file and added them to a standard data structure for later fetching.
  Args     : A file handle to a scores file, a reference to a hash
  Returns  : The number of sequence regions added to hashref
  
=cut

sub parseScores {
  my ( $FH, $seqListRef ) = @_;
  my $noSeqsRequested = 0;
  while (<$FH>) {
    if (/^(\S+)\s+(\S+)\/(\d+)\-(\d+)/) {
      push( @{ $$seqListRef{$2} }, { start => $3, end => $4 } );
      $noSeqsRequested++;
    }
    else {
      die "Failed to parse line in scores file: [$_]\n";
    }
  }
  return ($noSeqsRequested);
}

=head2 parseList 

  Title    : parseList
  Usage    : Bio::Pfam::SeqFetch::parseList(\*L, \%seqsList)
  Function : Parse a file containing a list of  inf the format of accession/start-end 
           : and added them to a standard data structure for later fetching.
  Args     : A file handle to a GDF file, a reference to a hash
  Returns  : Nothing - we are using hashes
  
=cut

sub parseList {
  my ( $FH, $seqListRef ) = @_;
  my $noSeqsRequested = 0;
  while (<$FH>) {
    if (/(\S+)\/(\d+)-(\d+)/) {
      push( @{ $$seqListRef{$1} }, { start => $2, end => $3 } );
      $noSeqsRequested++;
    }
    elsif (/(\S+)/) {
      push( @{ $$seqListRef{$1} }, { whole => 1 } );
    }
    else {
      chomp;
      print STDERR "Unrecognised line: [$_]\n";
    }
  }
  return ($noSeqsRequested);
}

#-------------------------------------------------------------------------------

=head2 addSeq 

  Title    : addSeq
  Usage    : Bio::Pfam::SeqFetch::addSeq($acc, $start, $end, \%seqList ) 
  Function : Added a sequence to hash
  Args     : Sequence accesion, start pos (optional), end pos (optional), a hashRef
           : For optional arguments use undef 
  Returns  : Nothing
  
=cut

sub addSeq {
  my ( $seq_name, $seq_beg, $seq_end, $seqListRef ) = @_;
  if ($seq_beg) {
    push( @{ $$seqListRef{$seq_name} },
      { start => $seq_beg, end => $seq_end } );
  }
  else {
    push( @{ $$seqListRef{$seq_name} }, { whole => 1 } );
  }
}

#-------------------------------------------------------------------------------

=head2 fetchSeqs 

  Title    : fetchSeqs
  Usage    : Bio::Pfam::SeqFetch::fetchSeqs
  Function : Fetches sequencs using esl-sfetch
  Args     : hashRef of sequences in the expected data structure, 
           : location of xdfromated sequence db,
           : Filehandle (if not supplies, will use STDOUT)
  Returns  : The number of sequences fetched.
  
=cut

sub fetchSeqs {
  my ( $seqListRef, $index, $FH ) = @_;

  $FH = *STDOUT if not defined $FH;

  my $noSeqsFound = 0;
  my ( $tempFh, $filename ) = tempfile();
  foreach my $s ( keys %$seqListRef ) {
    print $tempFh "$s\n";
  }
  close($tempFh);

  open( XD, "esl-sfetch -f $index $filename |" )
    || die "Could not open esl-sfetch pipe:[$!]\n";
  my ( $seqId, $seq );
  while (<XD>) {
    if (/^>(\S+)/) {
      my $tmpSeqId = $1;

      if ($seqId) {
        foreach my $se ( @{ $$seqListRef{$seqId} } ) {
          if ( $se->{whole} ) {
            print $FH ">$seqId/1-" . length($seq) . "\n";
            print $FH "$seq\n";
            $noSeqsFound++;
          }
          else {
            my $tmpSeq = $seq;

            #check start >=1
            if ( $se->{start} < 1 ) {
              warn "start was " . $se->{start} . " now set to 1\n";
              $se->{start} = 1;
            }
            if ( $se->{end} <= length($tmpSeq) ) {
              my $domSeq = substr(
                $tmpSeq,
                $se->{start} - 1,
                $se->{end} - $se->{start} + 1
              );
              $noSeqsFound++;
              print $FH ">$seqId/" . $se->{start} . "-" . $se->{end} . "\n";
              print $FH "$domSeq\n";
            }
            else
            { ###Recently changed to from just a warning to reporting the whole sequence or $start-end depeding on $start
              warn "Length mismatch for $seqId/"
                . $se->{start} . "-"
                . $se->{end}
                . "length: "
                . length($tmpSeq) . "\n";
              $se->{end} = length($tmpSeq);
              my $domSeq = substr(
                $tmpSeq,
                $se->{start} - 1,
                $se->{end} - $se->{start} + 1
              );
              $noSeqsFound++;
              print $FH ">$seqId/" . $se->{start} . "-" . $se->{end} . "\n";
              print $FH "$domSeq\n";

            }
          }
        }    #End for each
      }
      $seqId = $tmpSeqId;
      $seq   = "";
    }
    else {
      chomp;
      $seq .= $_;
    }
  }
  close(XD);

  if ($seqId) {
    foreach my $se ( @{ $$seqListRef{$seqId} } ) {
      if ( $se->{whole} ) {
        print $FH ">$seqId/1-" . length($seq) . "\n";
        print $FH "$seq\n";
        $noSeqsFound++;
      }
      else {
        my $tmpSeq = $seq;

        #check start >=1
        if ( $se->{start} < 1 ) {
          warn "start was " . $se->{start} . " now set to 1\n";
          $se->{start} = 1;
        }
        if ( $se->{end} <= length($tmpSeq) ) {
          my $domSeq =
            substr( $tmpSeq, $se->{start} - 1, $se->{end} - $se->{start} + 1 );
          $noSeqsFound++;
          print $FH ">$seqId/" . $se->{start} . "-" . $se->{end} . "\n";
          print $FH "$domSeq\n";
        }
        else
        { ###Recently changed to from just a warning to reporting the whole sequence or $start-end depeding on $start
          warn "Length mismatch for $seqId/"
            . $se->{start} . "-"
            . $se->{end}
            . "length: "
            . length($tmpSeq) . "\n";
          $se->{end} = length($tmpSeq);
          my $domSeq =
            substr( $tmpSeq, $se->{start} - 1, $se->{end} - $se->{start} + 1 );
          $noSeqsFound++;
          print $FH ">$seqId/" . $se->{start} . "-" . $se->{end} . "\n";
          print $FH "$domSeq\n";
        }
      }
    }    #End for each
  }
  return ($noSeqsFound);
}

#-------------------------------------------------------------------------------

=head2 addSeqToVerify 

  Title    : addSeqToVerify
  Usage    : Bio::Pfam::SeqFetch::addSeqToVerify
  Function : This is very similar to the addSeq method, except it includes the sequence
  Args     : accessions, start, end, sequence, hashref
  Returns  : Nothing
  
=cut

sub addSeqToVerify {
  my ( $seq_name, $seq_beg, $seq_end, $seq, $seqListRef ) = @_;
  unless ( $seq_name and $seq_beg and $seq_end and $seq ) {
    die
"Need to specify seqname [$seq_name], seq_beg [$seq_beg] and seq_end [$seq_end]\n";
  }
  push(
    @{ $$seqListRef{$seq_name} },
    { start => $seq_beg, end => $seq_end, seq => $seq }
  );

}

#-------------------------------------------------------------------------------

=head2 verifySeqs 

  Title    : verifySeqs
  Usage    : Bio::Pfam::SeqFetch::verifySeqs(\%mySeqs, $pathToSeqs, \*FH) 
  Function : Fetches the sequences and make sure they are the same
  Args     : hashRef, pfamseqLocation, filehandle
  Returns  : noSequences cross referenced
  
=cut

sub verifySeqs {
  my ( $seqListRef, $index, $FH ) = @_;

  $FH = *STDOUT if not defined $FH;

  my $noSeqsFound = 0;
  my ( $tempFh, $filename ) = tempfile();
  foreach my $s ( keys %$seqListRef ) {
    print $tempFh "$s\n";
  }
  close($tempFh);

  open( XD, "esl-sfetch -f $index $filename |" )
    || die "Could not open esl-sfetch pipe:[$!]\n";

  my ( $seqId, $seq );
  while (<XD>) {
    if (/^>(\S+)/) {
      my $tmpSeqId = $1;

      if ($seqId) {
        foreach my $se ( @{ $$seqListRef{$seqId} } ) {
          my $tmpSeq = $seq;
          my $domSeq =
            substr( $tmpSeq, $se->{start} - 1, $se->{end} - $se->{start} + 1 );
          if (  ( ( $se->{end} - $se->{start} + 1 ) eq length($domSeq) )
            and ( $domSeq eq $se->{seq} ) )
          {
            $noSeqsFound++;
          }
          else {
            print "Sequence $seqId/"
              . $se->{start} . "-"
              . $se->{end}
              . " does not match the database\n";
          }

        }    #End for each
      }
      $seqId = $tmpSeqId;
      $seq   = "";
    }
    else {
      chomp;
      $seq .= $_;
    }
  }
  close(XD);

  if ($seqId) {
    foreach my $se ( @{ $$seqListRef{$seqId} } ) {
      my $tmpSeq = $seq;
      my $domSeq =
        substr( $tmpSeq, $se->{start} - 1, $se->{end} - $se->{start} + 1 );
      if (  ( ( $se->{end} - $se->{start} + 1 ) eq length($domSeq) )
        and ( $domSeq eq $se->{seq} ) )
      {
        $noSeqsFound++;
      }
      else {
        print "Sequence $seqId/"
          . $se->{start} . "-"
          . $se->{end}
          . " does not match the database\n";
      }
    }    #End for each
  }

  return ($noSeqsFound);
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

File: SeqFetch.pm

Copyright (c) 2007: Genome Research Ltd.

Author: Rob Finn (rdf@sanger.ac.uk)

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
