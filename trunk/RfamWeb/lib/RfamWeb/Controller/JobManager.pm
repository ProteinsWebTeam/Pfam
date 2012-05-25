
# JobManager.pm
# jt6 20070817 WTSI
#
# $Id: JobManager.pm,v 1.1 2008-09-12 09:13:10 jt6 Exp $

=head1 NAME

RfamWeb::Controller::JobManager - some helper methods for submitting jobs

=cut

package RfamWeb::Controller::JobManager;

=head1 DESCRIPTION

This controller is responsible for running sequence searches. This class is an
empty sub-class of the PfamBase class, where all the action is.

$Id: JobManager.pm,v 1.1 2008-09-12 09:13:10 jt6 Exp $

=cut

use Moose;
use namespace::autoclean;

BEGIN {
  extends 'Catalyst::Controller';
}

with 'PfamBase::Roles::JobManager';

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
