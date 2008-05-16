package Bio::Pfam::Queues::IntQueue;
#########
# Author:        rdf
# Maintainer:    rdf
# Created:       2007-04-05
# Last Modified: $Date: 2008-05-16 15:13:18 $
# Id:            $Id: IntQueue.pm,v 1.3 2008-05-16 15:13:18 jt6 Exp $
#
# Based on SimpleDB written by Roger Pettett and Jody Clements.
# Performs Pfam single sequence search database.
# Database-backed queuing using roughly the same API as LSF
#
# command   = target executable
# priority  = <queuename from subclass>
#
use strict;
use warnings;
use Sys::Hostname;
use English qw(-no_match_vars);
use Carp;
use Data::UUID;
use Data::Dumper;
use File::Basename;
#use Config::General;
use POSIX qw(setsid);
use Bio::Pfam::PfamJobsDBManager;

sub new {
  my ( $caller, $ref ) = @_;
  my $self = {};
  
  my $class = ref($caller) || $caller;
  print STDERR "$class\n";
  #Bless $self
 if($class){
    bless($self, $class);
  }else{
    die "Could not bless object\n";  
  }
  
  
  #All of this should really go into some sort of config
  #Lost of current jobs that can be dealt with	
  $self->knownJobs( { view => 'makepfamview.pl' } );
  #Get a database connection
  $self->getSchema({password => 'mafp1' });
  #Start setting up the local information for setting up the jobs
  $self->tmpDir("/lustre/scratch1/sanger/pfam/");
  $self->currentDir("/lustre/pfam/pfam/Production/Pfam/CURRENT");
  $self->farmNode("farm-login");
  $self->hugeMemNode("turing");
  self->hugeMemTmpDir("/tmp");
  #Return the blessed object
  return ($self);
}


sub getCommand{
	my($self, $job_type) = @_;
	if($self->{'jobTypes'}->{$job_type}){
		return $self->{'jobTypes'}->{$job_type};
	}else{
		warn "Unrecognised Job type\n";
	}
}

sub knownJobs{
	my ($self, $jobRefs) = @_;
	if($jobRefs and ref($jobRefs) eq "HASH"){
		$self->{'jobTypes'} = $jobRefs
	}
}

sub hugeMemNode{
	my ($self, $farmNode) = @_;
	if($farmNode){
		$self->{'hugeMemNode'} = $farmNode
	}
	return $self->{'hugMemNode'};
}

sub hugeMemTmpDir{
	my ($self, $tmpDir) = @_;
	if($tmpDir){
		$self->{'hugeMemTmpDir'} = $tmpDir;
	}
	return $self->{'hugMemTmpDir'};
}


sub farmNode{
	my ($self, $farmNode) = @_;
	if($farmNode){
		$self->{'farmNode'} = $farmNode
	}
	return $self->{'farmNode'};
}

sub knownJobTypes{
	my $self = shift;
	if($self->{'jobTypes'}){
		return (keys %{$self->{'jobTypes'}});
	}
}

sub tmpDir {
	my ($self, $tmpDir) = @_;
	if($tmpDir){
		$self->{'tmpDir'} = $tmpDir;
	}
	return $self->{'tmpDir'};
}

sub currentDir {
	my ($self, $cDir) = @_;
	if($cDir){
		$self->{'currentDir'} = $cDir;
	}
	return $self->{'currentDir'};
}

#sub job_type {
#  my ($self, $job_type) = @_;
#  my $id;
#  if($self->tracker()) {
#    ($id) = split /:/mx, $self->tracker();
#  }elsif($id) {
#    my $result = $self->getSchema
#                       ->resultset("jobHistory")
#                        ->find({id => $id});
#   
#    if( $result->job_type ) {
#      $self->{'command'} = $self->getCommand;
#    }
#  }
#  return $self->{'command'};
#}

sub getSchema {
  my ($self, $connectionParams) = @_;

  if(!$self->{'dbSchema'}) {
    my $pfamJobsDB = Bio::Pfam::PfamJobsDBManager->new( %$connectionParams);
    $self->{'dbSchema'} = $pfamJobsDB->getSchema;
  }
  return $self->{'dbSchema'};
}

sub statuses {
  return sort qw(DONE FAIL SUB PEND RUN);
}

sub submit {
  my ($self)  = @_;
  $self->presubmit();
  my $host    = hostname();
  my $block   = ($self->blocking() && $self->blocking() ne 'no')?1:0;

  
    $self->debug() and print {*STDERR} "insert new job PEND\n";
    my $resultHistory = $self->getSchema
                        ->resultset('JobHistory')
                          ->create({'command' => $self->command,
                                    'priority' => $self->priority,
                                    'status'  => 'PEND',
                                    'opened'  => \'NOW()' });
                                    
    my $trackerid = $resultHistory->id;
    print STDERR "tracker id = $trackerid\n";
    my $resultStream = $self->getSchema
                              ->resultset('JobStream')
                                ->create({'id' => $resultHistory->id,
                                          
                                         'stdin' => $self->stdin() || q() }); 
    $self->debug() and print {*STDERR} "insert $trackerid stdin data\n";
    $self->tracker("$trackerid:");
    
    if($self->debug){
        print STDERR "***** RUNNING QUERY *****\n";
       my @rs = $self->getSchema
                      ->resultset( "JobHistory" )
                        ->search( { status => "PEND",
                                  id     => { '<',  $resultStream->id },
                                  job_id => { 'not like', $resultStream->job_id }
                        
                                   },
                                  { select => [
                                     { count => "id" }
                                  ],
                                  as     => [ 'num' ] }
                                     );
                                     print STDERR "***** RUN QUERY: Pending jobs=".$rs[0]->get_column("num")."*****\n";
    }  
        
  if($block) {
    # time out?
    while(1) {
      if($self->is_complete()) {
	last;

      } else {
	#########
	# enforced delay so we're not whizzing around in our tight loop
	#
	sleep 1;
      }
    }
  }

  #$self->postsubmit();

  $self->debug() and print {*STDERR} qq(submit returning tracker=@{[$self->tracker()]}\n);
  my ($id) = split /:/mx, $self->tracker();
  return $id;
}

#Done
sub status {
  my ($self, $id) = @_;
  my $status = q();
  if(!$id && $self->tracker()) {
    ($id) = split /:/mx, $self->tracker();
  }
 
 my $result = $self->getSchema
                    ->resultset('JobHistory')
                     ->find({'id' => $id});         
  return ($result->status);
}

#Done
sub is_complete {
  my ($self, $index) = @_;
  return ($self->status($index) !~ /PEND|RUN|SUSP/mx);
}

sub satisfy_pending_job {
  my ($self) = @_;
  return $self->satisfy_pending_jobs(1);
}

sub numberPendingJobs {
	my($self, $email ) = @_;
	print STDERR "Calculating priority for $email\n";
	my $result = $self->getSchema
					->resultset('JobHistory')
						->find( {email  => [ $email],
								 status => [ qw( PEND RUN ) ] },
								{select => [
									{ count => "email" },
									],
									as =>  'count'
									}
									);
	my $c = $result->get_column('count');
	return $c;
}

sub satisfy_pending_jobs {
  my ($self, $num) = @_;
  $num ||= 1;
  
  #Get a list of the jobs
  my @results = $self->getSchema
                      ->resultset('JobHistory')
                       ->search({status => "PEND",
                                 job_type => [$self->knownJobTypes] },
                                {order_by => 'me.id ASC' });
   #Take the first job
   my $result = shift @results;
  
    
   if($result){
      return ( {
      'id'         => $result->id,
      'job_id'     => $result->job_id,
      'status'     => $result->status,
      'job_type'   => $result->job_type,
      'family_id'  => $result->family_id,
      'family_acc' => $result->family_acc,
      'family_size' => $result->family_size,
      'options'    => $result->options,
      'user_id'    => $result->user_id,
      'command'    => $self->getCommand($result->job_type)
    });
  }
}




sub get_job {
	my ($self, $id) = @_;
	my $result;
	
	if($id){
		$result = $self->getSchema
                      ->resultset('JobHistory')
                       ->find({  status   => "PEND",
                       		     id       => $id },
                               { join     => [ qw( job_stream )],
                                 prefetch => [ qw( job_stream )] });
				
	}

	if($result){
		return ( 
    	{
      		'id'      => $result->id,
      		'job_id'  => $result->job_id,
      		'job_type'=> $result->job_type,
      		'options' => $result->options,
      		'stdin'   => $result->stdin,
      		'stderr'  => $result->stderr,
      		'status'  => $result->status,
      		'email'   => $result->email,
      		'command' => $self->getCommand($result->job_type)
    });
		
	}

}




sub update_job_status {
  my ($self, $id, $status) = @_;

  if(!$id && $self->tracker()) {
    ($id) = split /:/mx, $self->tracker();
  }
  
  my $result = $self->getSchema
                  ->resultset('JobHistory')
                    ->find({id => $id }); 
  
  if($status =~ /^(DONE|FAIL)$/mx) {
    $result->update({ closed => \'NOW()'});
  }elsif($status eq 'RUN') {
    $result->update({ started => \'NOW()'});
  }
  
  $result->update({status => $status});
  return;
}

#Done
sub job_stream {
  my ($self, $streamid, $id) = @_;
  
  if(!$id && $self->tracker()) {
    ($id) = split /:/mx, $self->tracker();
  }

  if($streamid !~ /^std(in|out|err)$/mx) {
    carp qq(PfamSSSDB::update_job_stream: Invalid stream '$streamid');
    return q();
  }

  my $results = $self->getSchema
                      ->resultset('JobStream')
                        ->find({ id => $id } );
  
  return $results->$streamid || q();
}



#Done
sub update_job_stream {
  my ($self, $id, $streamid, $content) = @_;
  
  if(!$id && $self->tracker()) {
    ($id) = split /:/mx, $self->tracker();
  }

  if($streamid !~ /^std(out|err)$/mx) {
    carp qq(PfamSSSDB::update_job_stream: Invalid stream '$streamid');
    return -1;
  }

  my $results = $self->getSchema
                      ->resultset('JobStream')
                        ->find({ id => $id } );
  $results->update({ $streamid => $content||q()});
  return;
}


sub daemonise {
  my $self = shift;
  chdir '/var/lock'                 or die "Can't chdir to /: $!";
  umask 0;
  open STDIN, '/dev/null'   or die "Can't read /dev/null: $!";
  open STDOUT, '>/dev/null' or die "Can't write to /dev/null: $!";
  open STDERR, '>/dev/null' or die "Can't write to /dev/null: $!";
  defined(my $pid = fork)   or die "Can't fork: $!";
  
  my $pidFile = basename($0);
  $pidFile =~ s/\.pl/\.pid/;
  
  if($pid){
    open PIDFILE, ">$pidFile" or die "can't open $pidFile: $!\n";
    print PIDFILE $pid;
    close PIDFILE;
    exit 0;
  }
  setsid                    or die "Can't start a new session: $!"; 
}

=head1 AUTHOR

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk)

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
