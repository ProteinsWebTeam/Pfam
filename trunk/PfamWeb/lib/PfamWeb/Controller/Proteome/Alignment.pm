
# Alignment.pm
# jt6 20070823 WTSI
#
# $Id: Alignment.pm,v 1.2 2007-08-28 15:03:46 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Proteome::Alignment - proteome alignments

=cut

package PfamWeb::Controller::Proteome::Alignment;

=head1 DESCRIPTION

This is the controller than handles the generation of alignments of
sequences from a given proteome.

$Id: Alignment.pm,v 1.2 2007-08-28 15:03:46 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Proteome';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 getAlignment : Private

Retrieves a sequence alignment from the job results table and formats the 
alignment ready to be marked up and displayed.

=cut

sub getAlignment : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Proteome::Alignment::getAlignment: retrieving alignment...' );

  # first get a job ID. The call to retrieve results will get the job ID for
  # itself, but we'll need it here anyway
  my( $jobId ) = $c->req->param('jobId') || '' =~ m/^([A-F0-9\-]{36})$/;

  unless( defined $jobId ) {
    $c->log->debug( 'Proteome::Alignment::getAlignment: no job ID found' );
    $c->stash->{errorMsg} = 'No job ID found for the sequence alignment job.';
    return;
  }   

  # retrieve the job results
  $c->forward( 'JobManager', 'retrieveResults' );
  unless( scalar keys %{ $c->stash->{results} } ) {
    $c->log->debug( 'Proteome::Alignment:::getAlignment: no results found' );
    $c->stash->{errorMsg} = 'No sequence alignment found.';
    return;
  }   

#  $c->log->debug( 'Family::Alignment::Builder:getAlignment: job results: |'
#                  . $c->stash->{results}->{$jobId}->{rawData} . '|' );

  # the rawData is just a string containing the alignment lines
  my @alignmentRows = split /\n/, $c->stash->{results}->{$jobId}->{rawData};
  
  # the consensus string is the last row of the alignment
  my $consensusString = pop @alignmentRows;
  
  # take a slice of that array, based on the "rows" setting from PfamViewer.
  # Rows are numbered from 1, not zero, so we need to offset the row values
  my $from = $c->stash->{rows}->[0] - 1;
  my $to   = $c->stash->{rows}->[1] - 1;
  $c->log->debug( 'Proteome::Alignment::getAlignment: showing rows |'
                  . "$from| to |$to|" );
  
  my %alignment;
  my $length;
  foreach ( @alignmentRows[ $from .. $to ] ) {
    # rows are either a consensus string or an alignment row
    if ( m|(\S+/\d+\-\d+)\s+(\S+)| ) {
      $alignment{$1} = $2;
      $length++;
    }  
  }
  
  # parse the consensus string
  my $consensus = Bio::Pfam::ColourAlign::parseConsensus( $consensusString );
 
  # stash everything
  $c->stash->{alignments}->{rawAlignments} = [ \%alignment ];
  $c->stash->{alignments}->{lengths}       = [ $length ];
  $c->stash->{alignments}->{consensus}     = [ $consensus ];
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

