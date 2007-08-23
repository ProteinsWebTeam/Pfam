
# Alignment.pm
# jt6 20070725 WTSI
#
# $Id: Alignment.pm,v 1.4 2007-08-23 16:16:46 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::Alignment - a base class for alignment handling

=cut

package PfamWeb::Controller::Family::Alignment;

=head1 DESCRIPTION

This is intended as the basis for alignment-related code.

$Id: Alignment.pm,v 1.4 2007-08-23 16:16:46 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Family';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 getAlignment : Private

Retrieves a family alignment, seed or full, from the DAS sources.

=cut

sub getAlignment : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Family::Alignment::getAlignment: retrieving alignment' );

  # set the DAS dsn based on the alignment type parameter
  if( $c->stash->{alnType} eq 'seed' ) {
    $c->stash->{dsn} = 'http://das.sanger.ac.uk/das/pfamSeedAlign';
  } else {
    $c->stash->{dsn} = 'http://das.sanger.ac.uk/das/pfamFullAlign';
  }

  $c->log->debug( 'Family::Alignment::getAlignment: dsn:  |' . $c->stash->{dsn} . '|' ); 
  $c->log->debug( 'Family::Alignment::getAlignment: acc:  |' . $c->stash->{acc} . '|' ); 
  $c->log->debug( 'Family::Alignment::getAlignment: rows: |' . $c->stash->{rows} . '|' ); 

  # retrieve the DasLite client from the base model class and hand it
  # the DSN
  my $dl = $c->model('PfamDB')->getDasLite;
  $dl->dsn( $c->stash->{dsn} );

  # put the rows specification into the right format for DAS
  my $rows = $c->stash->{rows}->[0] .'-' . $c->stash->{rows}->[1];

  # retrieve the raw alignment fragment and associated features via DAS and 
  # generate the consensus sequence 
  my $rawAlignment = $dl->alignment( { query => $c->stash->{acc},
                                       rows  => $rows } );
  my $features     = $dl->features( $c->stash->{acc} );
  my $consensus    = [ Bio::Pfam::ColourAlign::parseConsensus( getConsensus( $features ) ) ];
  
  #$c->log->debug( 'Family::Alignment::getAlignment: rawAlignment: ' . dump $rawAlignment );
  #$c->log->debug( 'Family::Alignment::getAlignment: features:     ' . dump $features );

  # build the marked-up alignment
  my( $alignments, $alignmentLengths ) = reconstructAlignment( $rawAlignment );

  # stash the arrays of alignments, alignment lengths and consensus strings  
  $c->stash->{alignments}->{rawAlignments} = $alignments;
  $c->stash->{alignments}->{lengths}       = $alignmentLengths;
  $c->stash->{alignments}->{consensus}     = $consensus;
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 reconstructAlignment

Reconstructs a blocked alignment from a raw alignment.

=cut

sub reconstructAlignment {
  my $ali = shift;
  my ($source, $aliData) = each %$ali;

  my( @alignments, @alignmentLengths );

  for( my $i = 0; $i < scalar(@$aliData); $i++ ) {
    my %aliObjects = 
      map{ $_->{alignobject_intObjectId} => $_ } @{ $aliData->[$i]->{alignobject} };
  
    push @alignmentLengths, $aliData->[$i]->{alignment_max};
  
    foreach my $block ( sort { $a->{block_blockOrder} <=> $b->{block_blockOrder} }
                             @{$aliData->[$i]->{block} } ) {
      my %ali;
      foreach my $bseqRef (@{ $block->{segment} } ) {
  
        my $key = $bseqRef->{segment_intObjectId} . '/' . 
                  $bseqRef->{segment_start}       . '-' . 
                  $bseqRef->{segment_end};
    
        $ali{$key} = getAlignmentString($bseqRef, \%aliObjects);
      }
      push @alignments, \%ali;
    }
  }
  return \@alignments, \@alignmentLengths;
}

#-------------------------------------------------------------------------------

=head2 getAlignmentString

Gets the alignment string from the alignment.

=cut

sub getAlignmentString{
  my( $bseqRef, $aliObjectsRef ) = @_;

  my $seqStr = $aliObjectsRef->{ $bseqRef->{segment_intObjectId} }->{sequence};

  my $seq = substr( $seqStr,
                    $bseqRef->{segment_start} - 1,
                    $bseqRef->{segment_end} - $bseqRef->{segment_start} + 1 );

  my $alignmentString = cigarToAlignment( $bseqRef->{cigar}, $seq );

  return $alignmentString;
}

#-------------------------------------------------------------------------------

=head2 cigarToAlignment

Converts a cigar string into an alignment row.

=cut

sub cigarToAlignment  {
  my $cigar = shift;

  $cigar =~ s/\"//g;

  my $seq = shift;
  my $tmp = $cigar;
  my $start = 0;
  my $len = length($seq);

  $tmp =~ s/(\d+)D/'-'x$1/eg;
  $tmp =~ s/D/\-/g;
  $tmp =~ s/(\d+)I/'.'x$1/eg;
  $tmp =~ s/I/\./g;

  $tmp =~ s/(\d{0,5})M/if($1){$start+=$1,($start<=$len)?substr($seq,$start-$1,$1):'~'x$1}else{$start+=1,($start<=$len)?substr($seq,$start-1,1):'~'}/eg;

  return $tmp;
}

#-------------------------------------------------------------------------------

=head2 getConsensus

Calculates the consensus sequence.

=cut

sub getConsensus {
  my $con = shift;

  my( $source, $featuresRef ) = each %$con;
  return $featuresRef->[0]->{feature_label};
}

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
