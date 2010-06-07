
# REST.pm
# pg6 2010/06/07
#
# $Id$

=head1 Name 

iPfamWeb::Controller::REST - Base controller for implementing REST-ful interface to the site.

=cut

package iPfamWeb::Controller::REST;

=head1 Description

This class is intended to be the base class for implementing the REST-ful interface to the whole
site. The configuration was declared in the package rather than in the separate file so that its
passed to the child class through the principle of inheritance.

=cut

use strict;
use warnings;
use Data::Dump qw( dump );
use JSON;

use base 'Catalyst::Controller::REST';

__PACKAGE__->config(
      'stash_key' => 'rest',
      'map'       => {
         'text/html'       => [ 'View', 'TT' ],
         'text/xml'        => 'XML::Simple',
         'text/x-yaml'        => 'YAML',
         'application/json'   => 'JSON',
         'text/x-json'        => 'JSON',
       }
  );

#-------------------------------------------------------------------------------

=head1 AUTHOR

Prasad Gunasekaran, C<pg6@sanger.ac.uk>

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
