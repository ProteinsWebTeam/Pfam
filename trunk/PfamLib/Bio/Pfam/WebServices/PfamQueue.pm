package Bio::Pfam::WebServices::PfamQueue;
#########
# Author:        rdf
# Maintainer:    rdf
# Created:       2007-04-05
# Last Modified: $Date: 2007-09-07 14:55:58 $
# Id:            $Id: PfamQueue.pm,v 1.4 2007-09-07 14:55:58 rdf Exp $
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
use Config::General;
use Bio::Pfam::WebUserDBManager;

sub new {
  my ($class, $queueType, $ref) = @_;
  my $self = {
	      '_fields' => [qw(debug
                           options
                           job_type
                           stdin
                           stderr
                           stdout
                           status
                           id
                           email
                           command)],
	     };
  bless $self, $class;
  
  my $conf;
  if($ENV{PFAMOFFLINE_CONFIG}){
    $conf = new Config::General($ENV{PFAMOFFLINE_CONFIG});    
  }else{
    die "Do not know where the config directory is\n"; 
  }
  my %config = $conf->getall;
  #print Dumper (%config);
  my $qConfig = $config{queue}->{$queueType}->{jobType};		
  $self->knownJobs($qConfig);
 
  $self->getSchema($config{Model}->{WebUser});
  
  $self->tmpDir($config{queue}->{$queueType}->{tmpDir});
  $self->dataFileDir($config{queue}->{$queueType}->{dataFileDir});
  $self->cpus($config{queue}->{$queueType}->{cpus}) if($config{queue}->{$queueType}->{cpus});
  $self->pvm($config{queue}->{$queueType}->{pvm}) if($config{queue}->{$queueType}->{pvm});
  $self->email($config{queue}->{$queueType}->{email}) if ($config{queue}->{$queueType}->{email});
  $self->thirdPartyQueue($config{queue}->{$queueType}->{thirdPartyQueue}) if ($config{queue}->{$queueType}->{thirdPartyQueue});
  for my $f ($self->fields()) {
    if(exists $ref->{$f}) {
      $self->{$f} = $ref->{$f};
    }
  }

  return $self;
}



sub fields {
  my $self = shift;
  return @{$self->{'_fields'}};
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

sub dataFileDir {
	my ($self, $dataFileDir) = @_;
	if($dataFileDir){
		$self->{'dataFileDir'} = $dataFileDir;
	}
	return $self->{'dataFileDir'};
}

sub pvm {
	my ($self, $pvm) = @_;
	if($pvm){
		$self->{'pvm'} = $pvm;
	}
	return $self->{'pvm'} || 0;
}

sub email {
	my ($self, $email) = @_;
	print STDERR Dumper($email);
	if($email){
		$self->{'email'} = $email;
	}
	return $self->{'email'};
}

sub thirdPartyQueue {
	my ($self, $queue) = @_;
	if($queue){
		$self->{'3Pqueue'} = $queue->{location};
	}
	return $self->{'3Pqueue'};
}
sub cpus {
	my ($self, $cpus) = @_;
	if($cpus){
		$self->{'cpus'} = $cpus;
	}
	return $self->{'cpus'} || 1;
}

sub job_type {
  my ($self, $job_type) = @_;
  my $id;
  if($self->tracker()) {
    ($id) = split /:/mx, $self->tracker();
  }

  

   elsif($id) {
    my $result = $self->getSchema
                       ->resultset("jobHistory")
                        ->find({id => $id});
    if($result->command =~ /hmmer/) {
      $self->{'command'} = $result->command;
    }
  }
  return $self->{'command'};
}

sub getSchema {
  my ($self, $connectionParams) = @_;

  if(!$self->{'dbSchema'}) {
    my $webUserDB = Bio::Pfam::WebUserDBManager->new( %$connectionParams);
    $self->{'dbSchema'} = $webUserDB->getSchema;
  }

  return $self->{'dbSchema'};
}

sub statuses {
  return sort qw(DONE FAIL PEND RUN);
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
								 status => [ qw/PEND RUN/ ] },
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
  $num           ||= 1;
  
  
  my @results = $self->getSchema
                      ->resultset('JobHistory')
                       ->search({status => "PEND",
                                 job_type => [$self->knownJobTypes] },
                                {join     => [ qw/job_stream/],
                                 prefetch => [ qw/job_stream/],
                                 order_by => 'me.id ASC' });
   my $result = shift @results;
   if($result){
    $result->update( { status  => 'RUN',
                      started => \'NOW()'});
                                   
                          


  return ( 
    {
      'id'      => $result->id,
      'job_id'  => $result->job_id,
      'stdin'   => $result->stdin,
      'status'  => $result->status,
      'job_type' => $result->job_type,
      'options' => $result->options,
      'email'   => $result->email,
      'command' => $self->getCommand($result->job_type)
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
                               { join     => [ qw/job_stream/],
                                 prefetch => [ qw/job_stream/] });
				
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














#Deal with this stuff later!














# sub job_times {
#   my ($self, $id) = @_;
#   my $q           = $self->queue();
#   my $dbh         = $self->dbh();
#   my $ref         = [];
# 
#   if(!$id && $self->tracker()) {
#     ($id) = split /:/mx, $self->tracker();
#   }
# 
#   eval {
#     my $sth = $dbh->prepare(qq(SELECT opened,started,closed,
#                                       TIMEDIFF(closed,started) AS wallclock,
#                                       TIMEDIFF(closed,opened)  AS total,
#                                       TIMEDIFF(started,opened) AS waiting
#                                FROM   ${q}_history
#                                WHERE  id=?));
#     $sth->execute($id);
#     $ref = $sth->fetchrow_hashref();
#     $dbh->commit();
#   };
# 
#   if($EVAL_ERROR) {
#     carp $EVAL_ERROR;
#     $dbh->rollback();
#     return {};
#   }
# 
#   return $ref;
# }
# sub statistics {
#   my ($self, $id) = @_;
#   my ($position, $pending, $running, $throughput);
#   my $q           = $self->queue();
#   my $dbh         = $self->dbh();
# 
#   if(!$id && $self->tracker()) {
#     ($id) = split /:/mx, $self->tracker();
#   }
# 
#   eval {
#     $pending    = $dbh->selectall_arrayref(qq(SELECT count(*)
#                                               FROM   ${q}_history
#                                               WHERE  status='PEND'));
# 
#     $running    = $dbh->selectall_arrayref(qq(SELECT count(*)
#                                               FROM   ${q}_history
#                                               WHERE  status='RUN'));
#     $throughput = $dbh->selectall_arrayref(qq(SELECT COUNT(id) /
#                                                      TIME_TO_SEC(
#                                                       TIMEDIFF(
#                                                        MAX(closed),
#                                                        MIN(closed)
#                                                       )
#                                                      )
#                                               FROM   ${q}_history
#                                               WHERE  closed > current_date));
# 
#     if($id) {
#       $position = $dbh->selectall_arrayref(qq(SELECT count(*)
#                                               FROM   ${q}_history
#                                               WHERE  status='PEND'
#                                               AND    id < ?), {}, $id);
#     }
#     $dbh->commit();
#   };
#   if($EVAL_ERROR) {
#     carp $EVAL_ERROR;
#     $dbh->rollback();
#     return {};
#   }
# 
#   return {
# 	  'pending'    => $pending->[0]->[0]  || '0',
# 	  'running'    => $running->[0]->[0]  || '0',
# 	  'position'   => $position->[0]->[0] || '0',
# 	  'throughput' => $position->[0]->[0] || '0.00',
# 	 };
# }
# 
# sub statistics_history {
#   my ($self, $resolution, $limit) = @_;
#   my $q                   = $self->queue();
#   my $dbh                 = $self->dbh();
#   $resolution           ||= 'month';
#   my $date_fmt            = $DATE_FORMATS->{$resolution};
#   my $history             = [];
#   $limit                  = $limit?"LIMIT $limit":q();
#   eval {
#     $history = $dbh->selectall_arrayref(qq(SELECT date,status,count
#                                            FROM   (SELECT DATE_FORMAT(opened, '$date_fmt') AS date,
#                                                           status,
#                                                           COUNT(*) AS count
#                                                    FROM   ${q}_history
#                                                    WHERE  opened >= subdate(now(), interval 12 month)
#                                                    GROUP by date,status
#                                                    ORDER BY date DESC $limit) revdata
#                                            ORDER BY date));
#   };
#   carp $EVAL_ERROR if($EVAL_ERROR);
# 
#   my $munged = {};
#   my $attrs  = {};
# 
#   for my $row (@{$history}) {
#     $munged->{$row->[0]}->{$row->[1]} = $row->[2];
#     $attrs->{$row->[1]}++;
#   }
# 
#   return [map {
#     my $d = $_;
#     [$d,
#      map {
#        $munged->{$d}->{$_}||0;
#      } $self->statuses()
#     ];
#   } sort keys %{$munged}];
# }
# 
# sub statistics_wallclocktime_distribution {
#   my ($self, $resolution) = @_;
#   my $q                   = $self->queue();
#   my $dbh                 = $self->dbh();
#   $resolution           ||= 'hour';
#   my $date_fmt            = $DATE_FORMATS->{$resolution};
#   my $history             = [];
# 
#   eval {
#     my $query = qq(SELECT FLOOR( d1 / l ) * l AS D, SUM(n1) AS N
#                    FROM        (SELECT   d1,
#                                          POWER(10, FLOOR( if(d1=0,1,log10(d1)))) AS l,
#                                          n1
#                                 FROM    (SELECT UNIX_TIMESTAMP(closed) - UNIX_TIMESTAMP(opened) AS d1,
#                                                 COUNT(*) AS n1
#                                          FROM   ${q}_history
#                                          WHERE  opened  != 0
#                                          AND    closed  != 0
#                                          AND    started != 0
#                                          GROUP BY d1) AS X1
#                                ) AS X2
#                    GROUP BY D);
# 
#     $history = $dbh->selectall_arrayref($query);
#   };
#   carp $EVAL_ERROR if($EVAL_ERROR);
#   return $history;
# }
# 
# sub statistics_avg_times {
#   my ($self, $resolution) = @_;
#   my $q                   = $self->queue();
#   my $dbh                 = $self->dbh();
#   $resolution           ||= 'month';
#   my $date_fmt            = $DATE_FORMATS->{$resolution};
#   my $history             = [];
# 
#   eval {
#     my $query = qq(SELECT DATE_FORMAT(opened, '%Y-%m') AS date,
#                           SUM(UNIX_TIMESTAMP(closed)  - UNIX_TIMESTAMP(started)) /COUNT(*) AS avg_execution_time,
#                           SUM(UNIX_TIMESTAMP(started) - UNIX_TIMESTAMP(opened))  /COUNT(*) AS avg_wait_time
#                    FROM  ${q}_history
#                    WHERE closed  != 0
#                    AND   started != 0
#                    GROUP BY date);
#     $history = $dbh->selectall_arrayref($query);
#   };
#   carp $EVAL_ERROR if($EVAL_ERROR);
#   return $history;
# }
# 
# sub DESTROY {
#   my $self = shift;
#   $self->getSchema->storage->disconnect;
# }

1;

__END__

=head1 NAME

Bio::Pfam::WebServices::PfamQueue - A transactional-database-backed queuing system superclass

=head1 VERSION

$Revision: 1.4 $

=head1 SYNOPSIS

 #########
 # subclass
 #
 package myqueue;
 use strict;
 use warnings;
 use base qw(Website::QueueStub::SimpleDB);

 sub preinit {
   my ($self) = @_;
   $self->priority('myqueue');
 }

 #########
 # submission client
 #
 my $oQueue = Website::QueueStub::SimpleDB::myqueue->new({
							  'command'  => 'myjob -r bar baz',
							 });
 my $iJobId $oQueue->submit();

 #########
 # execution server
 #
 my $oQueue = Website::QueueStub::SimpleDB::myqueue->new();

 while(1) {
   my $ref   = $oQueue->satisfy_pending_job();
   if($ref->{'id'}) {
     print "Executing id=$ref->{'id'}, command=$ref->{'command'}\n";

     $oQueue->update_job_status($ref->{'id'}, 'DONE');
   } else {
     sleep 5;
   }
 }

=head1 DESCRIPTION

The Website::QueueStub::SimpleDB:: family is designed to be a rough
equivalent to job submission management via LSF.

=head1 SUBROUTINES/METHODS

=head2 new : Constructor

 my $oQueue = Website::QueueStub::SimpleDB::myqueue->new({ ... });

 Options:
   debug        => 0          # debug on|off
   command      => ''         # whatever parameters your job requires
   priority     => 'myqueue'  # queuename, usually set up by a SimpleDB subclass (e.g. 'myqueue')
   masterhost   => 'webdbsrv' # optional overridden database host
   masterport   => 3306       # optional overridden database port
   masterdbname => ''         # optional overridden database name
   masterdbuser => ''         # optional overridden database username
   masterdbpass => ''         # optional overridden database password
   stdin        => ''         # standard input for job
   id           => ''         # job id, required for job retrieval
   blocking     => 0          # blocking i/o on|off

=head2 tracker : job tracker (use 'id' instead for non-LSF submissions)

 my $sTracker = $oQueue->tracker();

=head2 command : Get/set accessor

 my $command = $oQueue->command();
 $oQueue->command(q(command / job parameters));

=head2 AUTOLOAD

=head2 dbh : Get accessor for database handle

 my $oDbh = $oQueue->dbh();

=head2 queue : Get accessor for the queue jobs will run on

 my $sQueueName = $oQueue->queue();

=head2 queues : Get accessor for all available queues

 my @aQueueNames = $oQueue->queues();

=head2 statuses : Get accessor for status supported by these jobs

 my @aStatuses = $oQueue->statuses();

=head2 submit : Submit this job to the queue

 $oQueue->submit();

=head2 status : Query the status of this job (post-submission)

 my $sStatus = $oQueue->status();

=head2 is_complete : Basic status query testing if finished (post-submission)

 my $bJobFinished = $oQueue->is_complete();

=head2 satisfy_pending_job : Retrieve the next pending job from the queue

 my $hrJobData = $oQueue->satisfy_pending_job();

=head2 satisfy_pending_jobs : Retrieve a number of pending jobs from the queue

 my $arJobData = $oQueue->satisfy_pending_jobs();

=head2 update_job_status : Change the status of a job in the queue

 $oQueue->update_job_status($iJobId, $sStatus);

 Where $sStatus is from $oQueue->statuses();

=head2 job_stream : Get accessor for stream data

 my $sStreamData = $oQueue->job_stream($sStreamName);
 my $sStreamData = $oQueue->job_stream($sStreamName, $iJobId);

 Where $sStreamName is one of stdin|stdout|stderr

=head2 job_times : Retrieve started|opened|closed times for a job

 my $hrJobTimes = $oQueue->job_times();
 my $hrJobTimes = $oQueue->job_times($iJobId);

=head2 update_job_stream : Set accessor for updating stream data

 $oQueue->update_job_stream($iJobId, $sStreamName, $sContent);

=head2 statistics : Get accessor for general queue throughput statistics

 my $hrStatsData = $oQueue->statistics();

=head2 statistics_history

=head2 statistics_wallclocktime_distribution

=head2 statistics_avg_times

=head2 DESTROY : Destructor

 Cleans up database handles


=head1 DIAGNOSTICS

 $oQueue->debug(1);

=head1 CONFIGURATION AND ENVIRONMENT

=head1 DEPENDENCIES

 Website::QueueStub
 Website::Utilities::IdGenerator
 DBI

=head1 INCOMPATIBILITIES

=head1 BUGS AND LIMITATIONS

=head1 AUTHOR

Roger Pettett

=head1 LICENSE AND COPYRIGHT

=cut
