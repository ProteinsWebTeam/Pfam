
# JobManager.pm
# jt6 20070817 WTSI
#
# $Id: JobManager.pm,v 1.8 2009-10-07 14:18:35 jt6 Exp $

=head1 NAME

PfamBase::Roles::JobManager - some helper methods for submitting jobs

=cut

package PfamBase::Roles::JobManager;

=head1 DESCRIPTION

This role adds methods for running sequence searches.

$Id: JobManager.pm,v 1.8 2009-10-07 14:18:35 jt6 Exp $

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

use JSON;
use Data::Dump qw(dump);

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 checkStatus : Local

Returns the status of the specified job. Used by the javascript that polls for
the status via XMLHttpRequest calls.

=cut

sub checkStatus : Local {
  my ( $this, $c ) = @_;

  # build a hash that we'll convert into JSON and return
  $c->stash->{status} = {};

  my $jobId = $c->req->param( 'jobId' );
  unless ( $jobId =~ m/^([A-F0-9\-]{36})$/i ) {
    $c->log->debug( 'JobManager::checkStatus: bad job id' ) if $c->debug;
    $c->stash->{status}->{error} = 'Invalid job ID';
    $c->detach( 'returnStatus' );
  }

  # job ID appears to be valid; try querying for the status of that job
  my $jobHistory = $c->model( 'WebUser::JobHistory' )
                     ->find( { job_id => $jobId } );

  # make sure the query returned *something*
  if ( not defined $jobHistory ) {
    $c->log->debug( "JobManager::checkStatus: problem retrieving job status for job |$jobId|" )
      if $c->debug;
    $c->stash->{status}->{error} = 'Could not retrieve job status';
    $c->detach( 'returnStatus' );
  }

  # finally, check the real status 
  my %statusValues = ( PEND => 1,
                       RUN  => 1,
                       DONE => 1,
                       FAIL => 1 );
                       
  if ( $statusValues{ $jobHistory->status } ) {
    $c->log->debug( 'JobManager::checkStatus: job status is: |'
                    . $jobHistory->status . '|' )  if $c->debug;
    $c->stash->{status}->{status} = $jobHistory->status;
  }
  else {
    $c->log->error( q(JobManager::checkStatus: can't determine job status) );
    $c->stash->{status}->{status} = 'UNKNOWN';
  }

  # see how many jobs are pending
  my $rs = $c->model( 'WebUser::JobHistory' )
             ->search( { status   => 'PEND',
                         job_type => $jobHistory->job_type,
                         id       => { '<',        $jobHistory->id },
                         job_id   => { 'not like', $jobHistory->job_id } },
                       { select   => [
                                       { count => 'id' },
                                       { sum   => 'estimated_time' }
                                     ],
                         as       => [ qw( num wait ) ] }
                     );
  $c->stash->{status}->{numPending} = $rs->first()->get_column( 'num' );
  $c->stash->{status}->{waitTime}   = $rs->first()->get_column( 'wait' ) || 0;

  $c->log->debug( 'JobManager::checkStatus: found      |' .
                  $c->stash->{status}->{numPending} . '| pending jobs' )
    if $c->debug;
  $c->log->debug( 'JobManager::checkStatus: wait time: |' .
                  $c->stash->{status}->{waitTime} . '|' )
    if $c->debug;

  # add the times to the response
  $c->stash->{status}->{opened}  = $jobHistory->opened;
  $c->stash->{status}->{started} = $jobHistory->started;
  $c->stash->{status}->{closed}  = $jobHistory->closed;

  # and hand back that status
  $c->forward( 'returnStatus' );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 returnStatus : Private

Returns the status of a polled job as a JSON snippet. Short-circuits the default
end action because we're adding content directly to the response body. We also
take care to set apropriate headers to avoid this response being cached on the
client side.

=cut

sub returnStatus : Private {
  my( $this, $c ) = @_;

  # convert the status hash to a JSON object and return it
  $c->log->debug( 'JobManager::returnStatus: status: ' . $c->stash->{status} )
    if $c->debug;
  my $status = to_json( $c->stash->{status} );

  $c->log->debug( 'JobManager::returnStatus: returning: ' ) if $c->debug;
  $c->log->debug( dump( $c->stash->{status} ) ) if $c->debug;

  $c->res->content_type( 'application/x-json' );
  $c->res->body( $status );

  # make damned sure this isn't cached...
  $c->res->header( 'Pragma'        => 'no-cache' );
  $c->res->header( 'Expires'       => 'Thu, 01 Jan 1970 00:00:00 GMT' );
  $c->res->header( 'Cache-Control' => 'no-store, no-cache, must-revalidate,'.
                                      'post-check=0, pre-check=0, max-age=0' );

}

#-------------------------------------------------------------------------------

=head2 retrieveResults : Private

Populates the stash with the results of a specified job. Checks the C<Request>
parameters to find the jobId.

This is a legacy method, required for the Rfam searches.

=cut

sub retrieveResults : Private {
  my ( $this, $c, $jobId ) = @_;

  unless ( $jobId =~ m/^([A-F0-9\-]{36})$/i ) {
    $c->log->debug( "JobManager::retrieveResults: invalid job ID: |$jobId|" )
      if $c->debug;
    return;
  }

  $c->log->debug( "JobManager::retrieveResults: looking up details for job ID: |$jobId|" )
    if $c->debug;
  
  # job ID *looks* valid; try looking for that job
  my $job = $c->model( 'WebUser::JobHistory' )
              ->find( { job_id => $jobId },
                      { join     => [ qw( job_stream ) ],
                        prefetch => [ qw( job_stream ) ] } );
  
  # bail unless it exists
  return unless defined $job;
  
  # retrieve the results of the job and stash them
  $c->stash->{results}->{$jobId}->{job}          = $job;
  $c->stash->{results}->{$jobId}->{status}       = $job->status;
  $c->stash->{results}->{$jobId}->{rawData}      = $job->stdout;
  $c->stash->{results}->{$jobId}->{length}       = length( $job->stdin );
  $c->stash->{results}->{$jobId}->{method}       = $job->job_type;
  $c->stash->{results}->{$jobId}->{options}      = $job->options;

  $c->stash->{seq} = $job->stdin;

  $c->log->debug( "JobManager::retrieveResults: stashed results for |$jobId|" )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 retrieve_result_rows : Private

Populates the stash with the results of a specified job. Checks the C<Request>
parameters to find the jobId.

This is a new method, used by the new and improved Pfam searches. The new Pfam
search mechanism can have multiple jobs with the same job ID.

=cut

sub retrieve_result_rows : Private {
  my ( $this, $c, $jobId ) = @_;

  unless ( $jobId =~ m/^([A-F0-9\-]{36})$/i ) {
    $c->log->debug( "JobManager::retrieve_result_rows: invalid job ID: |$jobId|" )
      if $c->debug;
    return;
  }

  $c->log->debug( "JobManager::retrieve_result_rows: looking up details for job ID: |$jobId|" )
    if $c->debug;
  
  # job ID *looks* valid; try looking for that job
  my @jobs = $c->model( 'WebUser::JobHistory' )
               ->search( { job_id => $jobId },
                         { prefetch => [ qw( job_stream ) ] } );
  
  # bail unless one or more matching jobs exists
  return unless scalar @jobs;
  
  # retrieve the results of the jobs and stash them
  $c->stash->{results}->{$jobId}->{rows} = \@jobs;

  # my $job_data = { 
  #   job     => $job,
  #   status  => $job->status,
  #   rawData => $job->stdout,
  #   length  => length( $job->stdin ),
  #   method  => $job->job_type,
  #   options => $job->options,
  # }

  # the new searches store the user-specified options as a JSON object. Try
  # converting it back to a perl data structure, but handle the possibility
  # the the options were set by a job from the old (now Rfam only) search
  # system. If we get an exception when trying to convert from JSON, we just
  # ignore it, since the options were set by a search system that isn't 
  # planning to use the user_options anyway
  # eval {
  #   $c->stash->{results}->{$jobId}->{user_options} = from_json( $job->options );
  # };
  # if ( $@ ) {
  #   $c->log->debug( 'JobManager::retrieve_result_rows: could not convert options from JSON; no user options stashed' )
  #     if $c->debug;
  # }

  # $c->stash->{seq} = $job->stdin;

  $c->log->debug( 'JobManager::retrieve_result_rows: stashed results for ' . scalar @jobs
                  . " jobs with ID |$jobId|" )
    if $c->debug;
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
