
# JobStatus
# rdf 20070117 WTSI
#
# Model for the job_status table.
#
# $Id: JobStatus.pm,v 1.3 2007-03-16 11:25:25 jt6 Exp $
#
# $Author: jt6 $

package WebUser::JobStatus;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

# set up the table
__PACKAGE__->table( "job_status" );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw/job_id part part_name job_status internal_id/);

# set up the primary keys/contraints
__PACKAGE__->set_primary_key( "job_id" );

# relationships

__PACKAGE__->has_one( jobStatus => "WebUser::JobSubmission",
                      { "foreign.job_id" => "self.job_id"},
                      { proxy            => [ qw/job_unique_id
                                                 referrer
                                                 web_service_call 
                                                 parameters/] } );

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
