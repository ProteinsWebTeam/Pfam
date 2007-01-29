
# JobData
# rdf 20070117 WTSI
#
# Model for the job_data table.
#
# $Id: JobData.pm,v 1.1 2007-01-29 14:38:15 rdf Exp $

package WebUser::JobData;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

# set up the table
__PACKAGE__->table( "job_data" );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw/job_id data_id data_string/);

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
