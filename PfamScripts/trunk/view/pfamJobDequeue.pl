#!/software/bin/perl
#
# This code is desgined to act as a layer on top os LSF, taking jobs out of the
# pfam_jobs database and submit them to the farm.  Ideal, this would only encapsulate
# the job submission, but due to the complexity of some jobs, there is some 'logic'
# about dependencies included.  When run out of debug mode, the process will be forked
# and the parent process terminated so that the script runs in the background.
#
# Author        : rdf
# Maintainer    : $Author: rdf $
# Created       : 2008-05-05
# Last Modified : $Date: 2009-07-24 14:35:53 $
# Version       : $Revision: 1.5 $;
# Id            : $Id: pfamJobDequeue.pl,v 1.5 2009-07-24 14:35:53 rdf Exp $

use strict;
use warnings;
use Data::Dumper;
use IO::File;
use Getopt::Long;
use File::Rsync;
use LSF::Job;

# Our Module Found in Pfam-Core
use Bio::Pfam::Queues::IntQueue;

our $DEBUG = 1;

# Get a new queue stub
my $qsout = Bio::Pfam::Queues::IntQueue->new();

# Deamonise the process if we are not trying to debug the code
$qsout->daemonise unless ($DEBUG);

#Now set off the infinite loop!
while (1) {

  #See if there are any pending jobs
  my $ref = $qsout->satisfy_pending_job();

  $DEBUG && print Dumper( $ref, $qsout->{'jobTypes'} );

  #If there are not any jobs, see if there are any pending jobs
  unless ( $ref->{'id'} ) {
    sleep 15;    #Poll the database every 15 seconds....1 per LSF Poll
  }
  else {
    #Step 1 - There is a job to run!
    my $error = 0;
    my ( @cmds, $memory, $tmpDir, $queue, $resource );

    #Step 2 - Build up the resource requirements!
    if ( $ref->{'job_type'} eq "family" ) {
    #Build up the command here to run the view process!
    #Depending on the size of the family, dictates where the job should be scheduled to!
      if ( $ref->{'family_size'} ) {
        if ( $ref->{'family_size'} < 3000 ) {
          #Use small
          $resource = "select[mypfamlive<300 && type==X86_64] rusage[mypfamlive=10]";
          $tmpDir = $qsout->tmpDir;
          $queue  = 'small';
        }elsif( $ref->{'family_size'} >= 3000 and $ref->{'family_size'} < 20000 ){
          #Use normal
          $resource = 'select[mem>1500 && mypfamlive<300 && type==X86_64] rusage[mypfamlive=10:mem=1500]';
          $memory = '1500000'; #Request 1.5 GB of memory
          $tmpDir = $qsout->tmpDir;
          $queue  = 'normal';
        } else {
          #Higher memory requirements and run on long
          $memory = '7000000'; #Request 7 GB of memory
          $resource = 'select[type==X86_64 && mem>7000 && mypfamlive<300] rusage[mypfamlive=10:mem=7000]';
          $tmpDir = $qsout->tmpDir;
          $queue  = "long";
        }
      }
      else {

        #If we do not have a size of family, this will work in most cases.
        $memory = '7000000'; #We want 7GB of memory
        $resource = 'select[type==X86_64 && mem>7000 && mypfamlive<300] rusage[mypfamlive=10:mem=7000]';
        $tmpDir = $qsout->tmpDir;
        $queue  = 'long';
      }

      #Step 3 - Build up the command that we want to run

        my $cmd = $ref->{'command'};
        $cmd .= " -id " . $ref->{'job_id'};
        $cmd .= " -family " . $ref->{'entity_id'};
        $cmd .= " && rm -fr " . $tmpDir . "/" . $ref->{'job_id'};
        push( @cmds, $cmd );
 
    }elsif( $ref->{'job_type'} eq 'clan' ){
      #Repeat steps 2 and 3 if we have a clan view process to run.  
      #Step 2.2 - build up the LSF resource requirements
      $memory = '7000000'; #We want 7GB of memory
      $resource = 'select[type==X86_64 && mem>7000 && mypfamlive<300] rusage[mypfamlive=10:mem=7000]';
      #$host   = $qsout->farmNode;
      $tmpDir = $qsout->tmpDir;
      $queue  = 'long';
      #No sort out the command
      my $cmd = $ref->{'command'};
      push( @cmds, $cmd );
    }
    
    #Now submit the generic jobs using this method!
    foreach my $cmd (@cmds) {
      $DEBUG
        && print STDERR "Submitting id=$ref->{'id'}, command=$cmd, $error\n";

      #Now for the lsf bit
      if ($cmd) {

        #Now set up the lsf requirements
        
        
        my $mkAndCdToTmp = 'mkdir -p '. $tmpDir .'/'. $ref->{'job_id'} .'/'. $ref->{'entity_id'}. 
        ' && cd '.$tmpDir .'/'. $ref->{'job_id'} .'/'. $ref->{'entity_id'};
        
        $DEBUG && print STDERR "$mkAndCdToTmp && $cmd";

        my $job = LSF::Job->submit(
          -o => $tmpDir .'/'. $ref->{'job_id'} .'log',
          -q => $queue,
          -R => $resource,
          -M => $memory,
          "$mkAndCdToTmp && $cmd"
        );

        $qsout->update_job_with_lsf( $ref->{id}, $job->id );
        #$qsout->update_job_with_lsf( $ref->{id}, '10001' );

      }
      else {
        $error .= "unrecognised command line, commoand=|" . $cmd . "|\n";
      }
    }

    unless ($error) {
      $qsout->update_job_status( $ref->{id}, 'SUB' );
    }

    if ($error) {
      $qsout->update_job_status( $ref->{id}, 'FAIL' );
      $qsout->update_job_stream( $ref->{id}, 'stderr', $error );
      next;
    }
  }
}

exit(0);
