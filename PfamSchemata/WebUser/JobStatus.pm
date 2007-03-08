
# JobStatus
# rdf 20070117 WTSI
#
# Model for the job_status table.
#
# $Id: JobStatus.pm,v 1.2 2007-03-08 14:16:31 jt6 Exp $
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

1;
