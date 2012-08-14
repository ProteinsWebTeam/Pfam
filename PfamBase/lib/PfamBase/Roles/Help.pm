
# Help.pm
# jt6 20120514 WTSI
#
# $Id$

=head1 NAME

PfamBase::Roles::Help- role to help build the help pages

=cut

package PfamBase::Roles::Help;

=head1 DESCRIPTION

Role to help with building the help pages controllers.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

with 'PfamBase::Roles::Section';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Just sets up the look of the page. Tell the navbar where we are and set the
summary icons to "disabled".

=cut

sub begin : Private {
  my ( $this, $c ) = @_;

  $c->cache_page( 604800 );

  # tell the navbar where we are
  $c->stash->{nav} = 'help';

  # tell the layout template to disable the summary icons
  $c->stash->{iconsDisabled} = 1;
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
