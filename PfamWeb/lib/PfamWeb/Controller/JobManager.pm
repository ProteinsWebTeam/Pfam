
# JobManager.pm
# jt6 20070817 WTSI
#
# $Id: JobManager.pm,v 1.5 2008-01-07 14:00:09 jt6 Exp $

=head1 NAME

PfamWeb::Controller::JobManager - some helper methods for submitting jobs

=cut

package PfamWeb::Controller::JobManager;

=head1 DESCRIPTION

This controller is responsible for running sequence searches.

$Id: JobManager.pm,v 1.5 2008-01-07 14:00:09 jt6 Exp $

=cut

use strict;
use warnings;

use JSON;

use Data::Dump qw( dump );

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 checkStatus : Local

Returns the status of the specified job. Used by the javascript that polls for
the status via XMLHttpRequest calls.

=cut

sub checkStatus : Local {
  my( $this, $c ) = @_;

  # build a hash that we'll convert into JSON and return
  $c->stash->{status} = {};

  my $jobId = $c->req->param( 'jobId' );
  if( length( $jobId ) != 36 or $jobId !~ /^[A-F0-9\-]+$/ ) {
    $c->log->debug( 'JobManager::checkStatus: bad job id' );
    $c->stash->{status}->{error} = 'Invalid job ID';
    $c->detach( 'returnStatus' );
  }

  # job ID appears to be valid; try querying for the status of that job
  my $jobHistory = $c->model( 'WebUser::JobHistory' )
                     ->find( { job_id => $jobId } );

  # make sure the query returned *something*
  if( not defined $jobHistory ) {
    $c->log->debug( "JobManager::checkStatus: problem retrieving job status for job |$jobId|" );
    $c->stash->{status}->{error} = 'Could not retrieve job status';
    $c->detach( 'returnStatus' );
  }

  # finally, check the real status 
  my %statusValues = ( PEND => 1,
                       RUN  => 1,
                       DONE => 1,
                       FAIL => 1 );
                       
  if( $statusValues{ $jobHistory->status } ) {
    $c->log->debug( 'JobManager::checkStatus: job status is: |'
                    . $jobHistory->status . '|' );
    $c->stash->{status}->{status} = $jobHistory->status;
  } else {
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
                  $c->stash->{status}->{numPending} . '| pending jobs' );
  $c->log->debug( 'JobManager::checkStatus: wait time: |' .
                  $c->stash->{status}->{waitTime} . '|' );

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
  my $status = objToJson( $c->stash->{status} );

  $c->log->debug( 'JobManager::returnStatus: returning: ' );
  $c->log->debug( dump( $c->stash->{status} ) );

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

=cut

sub retrieveResults : Private {
  my( $this, $c, $job_id ) = @_;

  unless( $job_id =~ m/^([A-F0-9\-]{36})$/ ) {
    $c->log->debug( "JobManager::retrieveResults: looking up details for job ID: |$job_id|" );
    return;
  }

  $c->log->debug( "JobManager::retrieveResults: looking up details for job ID: |$job_id|" );
  
  # job ID *looks* valid; try looking for that job
  my $job = $c->model( 'WebUser::JobHistory' )
              ->find( { job_id => $job_id },
                      { join     => [ qw( job_stream ) ],
                        prefetch => [ qw( job_stream ) ] } );
  
  # bail unless it exists
  return unless defined $job;
  
  $c->log->debug( "JobManager::retrieveResults: stashing results for |$job_id|..." );
  
  # retrieve the results of the job and stash them
  $c->stash->{results}->{$job_id}->{rawData} = $job->stdout;
  $c->stash->{results}->{$job_id}->{length}  = length( $job->stdin );
  $c->stash->{results}->{$job_id}->{method}  = $job->job_type;
  $c->stash->{results}->{$job_id}->{options} = $job->options;
  $c->{stash}->{seq} = $job->stdin;
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
