
# PfamViewer.pm
# jt6 20060601 WTSI
#
# $Id: Viewer.pm,v 1.2 2007-08-23 16:22:57 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::Alignment::Viewer - viewing sequence alignments

=cut

package PfamWeb::Controller::Family::Alignment::Viewer;

=head1 DESCRIPTION

Various methods for viewing alignments.

$Id: Viewer.pm,v 1.2 2007-08-23 16:22:57 jt6 Exp $

=cut

use strict;
use warnings;

use Bio::Pfam::ColourAlign;

use JSON;
use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Family::Alignment';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 view : Local

Sets up the PfamViewer for this alignment.

=cut

sub showPfamViewer : Path {
  my( $this, $c ) = @_;

  # seed or full alignment
  if( $c->stash->{alnType} eq 'seed' ) {
    $c->stash->{dsn}                = 'http://das.sanger.ac.uk/das/pfamSeedAlign';
    $c->stash->{numRowsInAlignment} = $c->stash->{pfam}->num_seed;
  } else {
    $c->stash->{dsn}                = 'http://das.sanger.ac.uk/das/pfamFullAlign';
    $c->stash->{numRowsInAlignment} = $c->stash->{pfam}->num_full;
  }
  
  # build a "title" string, which will be used as the heading for the 
  # alignment tool window
  my $title = 'Pfam ' . $c->stash->{alnType} . ' alignment for '
              . $c->stash->{acc};
  
  $c->log->debug( 'Family::Alignment::Viewer::showPfamViewer: setting up getAlignment' );
  $c->stash->{params} = { source             => 'family',
                          title              => $title,
                          acc                => $c->stash->{acc},
                          alnType            => $c->stash->{alnType},
                          numRowsInAlignment => $c->stash->{numRowsInAlignment} };

  $c->log->debug( 'Family::Alignment::Viewer::showPfamViewer: forwarding...' );
  $c->forward( 'PfamViewer', 'showPfamViewer' );
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
