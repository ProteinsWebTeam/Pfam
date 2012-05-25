
# Help.pm
# jt6 20060925 WTSI
#
# $Id: Help.pm,v 1.1 2009-01-06 11:32:23 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Help - controller to build the help pages

=cut

package RfamWeb::Controller::Help;

=head1 DESCRIPTION

Displays the help pages for the RfamWeb site.

Generates a B<tabbed page>.

$Id: Help.pm,v 1.1 2009-01-06 11:32:23 jt6 Exp $

=cut

use Moose;
use namespace::autoclean;

BEGIN {
  extends 'Catalyst::Controller';
}

with 'PfamBase::Roles::Help';

# set the name of the section
__PACKAGE__->config( SECTION => 'help' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 section : Path

Override the default section-name action from the L<Section> role. We 
need to override to avoid "/help" returning the 404 page.

=cut

sub section : Path { }

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk), 
         Jennifer Daub (jd7@sanger.ac.uk)

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
