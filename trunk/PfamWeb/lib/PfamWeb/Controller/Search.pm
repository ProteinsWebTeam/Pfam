
# Search.pm
# jt6 20061108 WTSI
#
# $Id: Search.pm,v 1.24 2008-09-18 11:51:12 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search - top-level platform for performing various searches

=cut

package PfamWeb::Controller::Search;

=head1 DESCRIPTION

This controller is responsible for running searches. This is actually just an
empty wrapper around a shared base class. See
L<PfamBase::Controller::Search> for more details.

$Id: Search.pm,v 1.24 2008-09-18 11:51:12 jt6 Exp $

=cut

use strict;
use warnings;

# need to use Module::Pluggable here, otherwise the Search plugins don't 
# get correctly registered
use Module::Pluggable;

use base 'PfamBase::Controller::Search';

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
