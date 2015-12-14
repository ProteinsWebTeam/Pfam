
# Help.pm
# jt6 20060925 WTSI
#
# $Id: Help.pm,v 1.11 2009-10-28 14:01:00 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Help - controller to build the help pages

=cut

package PfamWeb::Controller::Help;

=head1 DESCRIPTION

Displays the help pages for the PfamWeb site.

Generates a B<tabbed page>.

$Id: Help.pm,v 1.11 2009-10-28 14:01:00 jt6 Exp $

=cut

use utf8;
use strict;
use warnings;

use base 'PfamWeb::Controller::Section';

# set the name of the section
__PACKAGE__->config( { SECTION => 'help' } );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Just sets up the look of the page. Tell the navbar where we are and set the
summary icons to "disabled".

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  $c->cache_page( 604800 );

  # tell the navbar where we are
  $c->stash->{nav} = 'help';

  # tell the layout template to disable the summary icons
  $c->stash->{iconsDisabled} = 1;
}

#-------------------------------------------------------------------------------

=head2 about : Global

Displays an "about" page.

=cut

sub about : Global {
  my ( $this, $c ) = @_;

  # tell the navbar where we are
  $c->stash->{nav} = 'about';

  $c->stash->{template} = 'pages/about.tt';
}

#-------------------------------------------------------------------------------

=head2 domain_graphics_example : Path

Displays a page with an example showing how to use the domain graphics library.

=cut

sub domain_graphics_example : Path('/help/domain_graphics_example.html') {
  my ( $this, $c ) = @_;

  $c->stash->{template} = 'pages/domain_graphics_example.tt';
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
