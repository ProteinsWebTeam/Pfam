
# BatchSearch.pm
# jt6 20061108 WTSI
#
# $Id: BatchSearch.pm,v 1.3 2008-03-19 14:44:53 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::BatchSearch - parent class for batch searches

=cut

package PfamWeb::Controller::Search::BatchSearch;

=head1 DESCRIPTION

This is the parent class for batch search operations.

$Id: BatchSearch.pm,v 1.3 2008-03-19 14:44:53 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Generates and stashes a job ID.

=cut

sub auto : Private {
  my( $this, $c ) = @_;
  
  # generate a job ID
  $c->stash->{jobId} = Data::UUID->new()->create_str();
 
}

#-------------------------------------------------------------------------------

=head2 queueSearch : Private

Queues a batch search.

=cut

sub queueSearch : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Search::BatchSearch::queueSearch: queueing a batch search to |'
                  . $c->stash->{job_type} . '|' ) if $c->debug;

  $c->log->debug( 'Search::BatchSearch::queueSearch: using command: |'
                  . $c->stash->{options} . '|' ) if $c->debug;

  # add this job to the tracking tables
  my $jobHistory = $c->model('WebUser::JobHistory')
                     ->create( { options        => $c->stash->{options},
                                 job_type       => $c->stash->{job_type},
                                 job_id         => $c->stash->{jobId},
                                 opened         => \'NOW()',
                                 status         => 'PEND',
                                 email          => $c->stash->{email} } );

  unless( defined $jobHistory ) { 
    $c->log->warn( "Search::BatchSearch::queueSearch: couldn't submit" )
      if $c->debug;
    $c->stash->{searchError} = 'There was a problem submitting your job.';
    return;
  }

  # add the sequences file to the data table     
  my $jobStream = $c->model('WebUser::JobStream')
                    ->create( { id    => $jobHistory->id,
                                stdin => $c->stash->{input} || q() } );

  unless( defined $jobStream ) { 
    $c->stash->{searchError} = 'There was a problem submitting your job.';
    return;
  }
  
  # check the submission time with a separate query
  my $historyRow = $c->model( 'WebUser::JobHistory' )
                     ->find( { id => $jobHistory->id } );

  # count the number of jobs in the queue before this one
  # see how many jobs are pending
  my $rs = $c->model( 'WebUser::JobHistory' )
             ->search( { status   => 'PEND',
                         job_type => $jobHistory->job_type,
                         id       => { '<',        $jobHistory->id },
                         job_id   => { 'not like', $jobHistory->job_id } },
                       { select   => [
                                       { count => 'id' },
                                     ],
                         as       => [ qw( num ) ] }
                     );
  $c->stash->{numPending} = $rs->first()->get_column( 'num' );

  # stash the job submission time
  $c->stash->{opened} = $historyRow->opened;
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
