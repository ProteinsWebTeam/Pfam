
# PfamViewer.pm
# jt6 20060601 WTSI
#
# $Id: Viewer.pm,v 1.5 2008-05-16 15:29:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::Alignment::Viewer - viewing sequence alignments

=cut

package PfamWeb::Controller::Family::Alignment::Viewer;

=head1 DESCRIPTION

Various methods for viewing alignments.

$Id: Viewer.pm,v 1.5 2008-05-16 15:29:28 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Family::Alignment';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 showPfamViewer : Path

Sets up the L<PfamWeb::Controller::PfamViewer> for this alignment. The only use 
for this controller is to add register a URL that shows the actual viewer, and 
to set up some initial parameters that are needed by the actual viewer.

This is the sequence of events that generates an alignment in the PfamViewer:

=over 4

=item 1

this action sets up some basic parameters for the PfamViewer:

=over 2

=item source

the source of the alignment; must be found in the enumerated list in the config
for this controller. Points to another controller which will actually return
or generate an alignment

=item title

the title for the pop-up window containing the alignment. A default value will
be used if it's not specified

=item acc

the accession for this family

=item alnType

seed or full

=item numRowsInAlignment

number of rows; required so that we can set up paging correctly

=back

=item 2

this action forwards to a private action on PfamViewer, which just pops up a 
new "tool" window

=item 3

an AJAX call in the tool window calls the "view" action on PfamViewer, handing
it the parameters that were first specified in this method

=item 4

the "view" action forwards to the source action specified here

=item 5

the source action populates the stash with the alignment

=item 6 

the PfamViewer "view" action marks up the alignment and hands off to a template
(alignmentFragment.tt)

=item 7

the template builds the page and adds the appropriate navigation links, which
are hooked into an AJAX call back to 
L<view|PfamWeb::Controller::PfamViewer::view> (step 3 in this list)

=back

=cut

sub showPfamViewer : Path {
  my ( $this, $c ) = @_;
  
  # build a "title" string, which will be used as the heading for the 
  # alignment tool window
  my $title = 'Pfam ' . $c->stash->{alnType} . ' alignment for '
              . $c->stash->{acc};
  
  # find out how many rows are in the alignment
  my $num_rows = ( $c->stash->{alnType} eq 'seed' )
                 ? $c->stash->{pfam}->num_seed
                 : $c->stash->{pfam}->num_full;
  
  $c->log->debug( 'Family::Alignment::Viewer::showPfamViewer: setting up getAlignment' )
    if $c->debug;

  $c->stash->{params} = { source             => 'family',
                          title              => $title,
                          acc                => $c->stash->{acc},
                          alnType            => $c->stash->{alnType},
                          numRowsInAlignment => $num_rows };

  $c->log->debug( 'Family::Alignment::Viewer::showPfamViewer: forwarding...' )
    if $c->debug;
  $c->forward( 'PfamViewer', 'showPfamViewer' );
}  

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
