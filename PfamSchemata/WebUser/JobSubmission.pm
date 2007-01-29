
# JobSubmission
# rdf 20070117 WTSI
#
# Model for the job_submission table.
#
# $Id: JobSubmission.pm,v 1.1 2007-01-29 14:38:16 rdf Exp $

package WebUser::JobSubmission;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

# set up the table
__PACKAGE__->table( "job_submission" );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw/job_id job_unique_id referrer web_service_call parameters/);

# set up the primary keys/contraints
__PACKAGE__->set_primary_key( "job_id" );

# relationships

__PACKAGE__->has_many( jobStatus => "WebUser::JobStatus",
		       { "foreign.job_id" => "self.job_id"},
		       { proxy            => [ qw/part 
                                                  part_name 
                                                  job_status 
                                                   internal_id/] } );

__PACKAGE__->has_many( jobData => "WebUser::JobData",
		       { "foreign.job_id" => "self.job_id"},
		       { proxy            => [ qw/data_id data_string/ ]});

__PACKAGE__->might_have( jobResults => "WebUser::JobResults",
			  { "foreign.job_id" => "self.job_id"});

1;
