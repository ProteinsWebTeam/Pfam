#
#===============================================================================
#
#         FILE:  DasViewer.pm
#
#  DESCRIPTION:  New PfamDasViewer which uses the alignment widget library to 
#                generate the alignments.
#
#        FILES:  ---
#         BUGS:  ---
#        NOTES:  ---
#       AUTHOR:  Prasad Gunasekaran ( pg6 @sanger.ac.uk )  
#       AUTHOR:  John Tate ( jt6 @sanger.ac.uk ) 
#       AUTHOR:  Rob Finn (rdf@sanger.ac.uk ) 
#      VERSION:  1.0
#      CREATED:  24/06/10 13:36:13
#     REVISION:  ---
#===============================================================================

package PfamWeb::Controller::Family::Alignment::DasViewer;

use strict;
use warnings;

use URI::Escape;
use JSON;
use Data::Dump qw( dump );

use base 'Catalyst::Controller';

sub showDasViewer: Path{
  my( $self, $c ) = @_;

  $c->log->debug('DasViewer::showDasViewer:start of the controller' );

  # build a "title" string, which will be used as the heading for the 
  # alignment tool window
  my $title = 'Pfam ' . $c->stash->{alnType} . ' alignment for '
              . $c->stash->{acc};
  
  # find out how many rows are in the alignment
  my $num_rows = ( $c->stash->{alnType} eq 'seed' )
                 ? $c->stash->{pfam}->num_seed
                 : $c->stash->{pfam}->num_full;
  
  my $dasSource = ( $c->stash->{ alnType } eq 'seed' ) ? 'Pfam_Seed_Alignments' : 'Pfam_Full_Alignments' ;
  $c->log->debug( 'Family::Alignment::DasViewer::showPfamDasViewer: setting up getAlignment' )
    if $c->debug;

  $c->stash->{params} = { source             => 'family',
                          dasSource          => $dasSource,
                          title              => $title,
                          acc                => $c->stash->{acc},
                          alnType            => $c->stash->{alnType},
                          numRowsInAlignment => $num_rows };
  
  # now stash the template which will have the necessary params to 
  $c->stash->{ template } = 'components/tools/pfamviewer/dasviewer.tt';
}
1;


