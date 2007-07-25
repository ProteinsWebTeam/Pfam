
# Search.pm
# jt6 20061108 WTSI
#
# $Id: Search.pm,v 1.16 2007-07-25 10:26:17 jt6 Exp $

=head1 NAME

PfamWeb::Controller::SeqSearch - perform various sequence searches

=cut

package PfamWeb::Controller::Search;

=head1 DESCRIPTION

This controller is responsible for running sequence searches.

$Id: Search.pm,v 1.16 2007-07-25 10:26:17 jt6 Exp $

=cut

use strict;
use warnings;

use Module::Pluggable;

use base 'PfamWeb::Controller::Section';

# set the name of the section
__PACKAGE__->config( SECTION => 'search' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Tries to extract the query terms from the URL and de-taint them.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  # tell the navbar where we are
  $c->stash->{nav} = 'search';
  
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
