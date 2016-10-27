#!/usr/bin/env perl
#
# Authors: Rob Finn & John Tate
#
# This script is designed to take jobs entered into the web_user database
# and run them.  This dequeuer will run non-interactive jobs. This is heavily
# tied to the WTSI LSF queue and will require tweaking for those wishing to run
# locally.  This performs some downweighting of users, but this is tied to LSF.
# If the config does not have a thirdParty queue set, then the jobs will be run by
# this dequeuer.
#
# The jobs that are non-interactive are:
# 1. single DNA sequence searches
# 2. batch protein searches
#
# In this script, there are three parts to the job:
# 1. Get the query data into a file so it is on the local machine.
# 2. Run the executable - pfam_scan.pl or pfamdna_search.pl
# 3. Once the job has finished the results are emailed to the users and entered in the database.
#
# There are no reasons that these could not be combined, but we use the pfam_scan.pl script elsewhere
# and want maintain it's current functionality. The fasta file writing in not performed in this
# dequeuer compared with fast- or hmmer- dequeues as at WTSI we use a third party LSF queue,
# and machine that this job runs on is different to the execution machine.
#
# Copyright (c) 2007: Genome Research Ltd.
#
# Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
#
# This is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

use strict;
use warnings;
use Data::Dumper;
use JSON;
use IPC::Cmd qw(run);
use File::Temp qw(tempfile);
use IO::File;
use IPC::Open3;
use IO::Handle;
use IO::Select;
use Getopt::Long;

# Our Module Found in Pfam-Core
use Bio::Pfam::WebServices::PfamQueue;

our $DEBUG = defined( $ENV{DEBUG} ) ? $ENV{DEBUG} : 1;

my $opts = {};

if ( defined $ENV{PIDFILE} ) {
  $opts->{pidfile} = $ENV{PIDFILE};
}

# Get a new queue stub
my $qsout = Bio::Pfam::WebServices::PfamQueue->new( "slow", $opts );
$qsout->daemonise unless ($DEBUG);

if ( $DEBUG && $ENV{PIDFILE} ) {
  open my $pid, '>', $ENV{PIDFILE}
    or die "Couldn't open pidfile: $!";
  print $pid $$;
  close $pid;
}

while (1) {
  my $job = $qsout->satisfy_pending_job();
  $DEBUG && print STDERR Dumper( $job, $qsout->{'jobTypes'} );

  unless ( $job->{'id'} ) {
    sleep 5;    #Poll the database every 5 seconds....3 per LSF Poll
  }
  else {
    my $error = 0;
    my ($cmd, $input);
    my ($fh, $tempfile) = tempfile(); 
    if ( $job->{'job_type'} eq "batch" ) {

      $cmd = "preJob.pl -id " . $job->{id} . " -tmp " . $qsout->tmpDir . " && ";
      my $opts;
      $cmd .= " " . $job->{'command'};
      if ( defined( $job->{options} ) and $job->{options} =~ /\S+/ ) {
        $opts = from_json( $job->{options} );
        $DEBUG
          && print STDERR 'dequeuer: options from db:' . Dumper($opts) . "\n";

        if ( $opts->{evalue} ) {
          $cmd .= " -e_seq " . $opts->{evalue} . " -e_dom " . $opts->{evalue};
        }

        if ( $opts->{batchOpts} ) {

          #Run both a Pfam-A and a Pfam-B search
          $cmd .= " -pfamB ";
        }
      }
      $cmd .= " -dir " . $qsout->dataFileDir;
      $cmd .= " -as";
      $cmd .= " -fasta " . $qsout->tmpDir . "/" . $job->{job_id} . ".fa";
      $cmd .= " > "
        . $qsout->tmpDir . "/"
        . $job->{job_id}
        . ".res 2> "
        . $qsout->tmpDir . "/"
        . $job->{job_id} . ".err";
      $cmd .= " ; postJob.pl -id " . $job->{id} . " -tmp " . $qsout->tmpDir;

    }
    elsif ( $job->{'job_type'} eq "dna" ) {
      $cmd = "preJob.pl -id " . $job->{id} . " -tmp " . $qsout->tmpDir . " && ";
      $cmd .= " " . $job->{'command'};
      $cmd .= " -in " . $job->{job_id} . ".fa";
      $cmd .= " -tmp " . $qsout->tmpDir;
      $cmd .= " -data " . $qsout->dataFileDir;
      $cmd .= " -cpu " . $qsout->cpus;
      $cmd .= " > "
        . $qsout->tmpDir . "/"
        . $job->{job_id}
        . ".res 2> "
        . $qsout->tmpDir . "/"
        . $job->{job_id} . ".err";
      $cmd .= " ; postJob.pl -id " . $job->{id} . " -tmp " . $qsout->tmpDir;
    }
    elsif ( $job->{job_type} eq 'rfam_batch' ) {

      #Get the users sequence from the database.
      #NOTE: there are currently
      $input = ">UserSeq\n" if $job->{stdin} !~ /^>/;
      $input .= $job->{stdin} . "\n";

      
      #my $infile   = $qsout->tmpDir . "/" . $job->{job_id} . ".in";
      #open(I, '>', $infile);
      #print I $input;
      #close(I); 
      
      my $outfile = $qsout->tmpDir . "/" . $job->{job_id} . ".res";

      #The command we want to run.
      $cmd =
          "cmscan -o $tempfile --cut_ga --cpu "
        . $qsout->rfcpus
        . " --FZ 5 --nohmmonly --notextw --tblout $outfile "
        . $qsout->rfamDataFileDir
        . "/Rfam.cm.1_1 - ; postJob.pl -id "
        . $job->{id}
        . " -tmp "
        . $qsout->tmpDir;
    }

    $DEBUG && print STDERR "Submitting id=$job->{'id'}, command=$cmd\n";

    #Now for the lsf bit
    if ($cmd) {
      $DEBUG && print STDERR "$cmd";

      if ( $qsout->thirdPartyQueue eq 'WTSI' ) {

        #Work out the Users priority
        my $c = $qsout->numberPendingJobs( $job->{'email'} );
        my $p = 50 - $c;
        $p = 1 if ( $p < 1 );

        #Now set up the LSF job
        my $fh = IO::File->new;
        $fh->open( "| bsub -q pfam_g2 -sp $p -n "
            . $qsout->cpus
            . " -R \"span[hosts=1]\"" );
        $fh->print("$cmd\n");
        $fh->close;

        #system($cmd);
      }
      else {
        executeCmd3( $cmd, $job, $qsout, $input );
      }

    }
    else {
      $error .= "unrecognised command line, commoand=$job->{'command'}\n";
    }

    if ($error) {
      $qsout->update_job_status( $job->{id}, 'FAIL' );
      $qsout->update_job_stream( $job->{id}, 'stderr', $error );
      next;
    }

  }
}

sub executeCmd3Test {
  my ( $cmd, $ref, $qsout, $input ) = @_;
  system($cmd);
}

sub executeCmd3 {
  my ( $cmd, $ref, $qsout, $input ) = @_;

#We are going to use IPC::Open3 to submit the search job. This allows us to pass the sequence in via STDIN.
#NOTE: HMMER/Infernal does this via the - field in the command line.

  #my ( $reader, $writer, $err ) =
  #  ( IO::Handle->new, IO::Handle->new, IO::Handle->new );
 
  my ($reader, $writer, $err);
  $writer = IO::Handle->new;

  $DEBUG && print "Going to run:$cmd\n";

  #Then the joys of open3 set up
  my $pid = undef;
  my $child_exit_status;
  eval {
    $pid = open3( $writer, $reader, $err, $cmd );

  };
  if ($@) {
    die "open3 failed: $!\n$@\n";
  }


  if ( defined($input) ) {
    print $writer $input;
  }
  close $writer;
  my(@stdout, @stderr);
  my $sel = new IO::Select;
  $sel->add($reader, $err);
  while(my @fhs = $sel->can_read) {
    foreach my $fh (@fhs) {
            my $line = <$fh>;
            unless(defined $line) {
                $sel->remove($fh);
                next;
            }
            if($fh == $reader) {
                push(@stdout, $line);
            }elsif($fh == $err) {
                push(@stderr, $line);
            }
        }
    } 

  #Put anything we have captured in stdout and stderr back in the database
  if ( scalar(@stdout) ) {
    $qsout->update_job_stream( $ref->{id}, 'stdout', join "", @stdout );
  }
  close($reader);

  if ( scalar(@stderr) ) {
    $qsout->update_job_stream( $ref->{id}, 'stderr', join "", @stderr );
  }

#Failing to do this can result in an accumulation of defunct or "zombie" processes.
#Only do it when we have closed all of the IO::Handles
  waitpid( $pid, 0 );

#Determine the exist status of the child process.
#We can also check stderr.  Anything in either will indicate some sort of issue.
  $child_exit_status = $? >> 8;
  my $success = 0;
  if ( $child_exit_status or scalar(@stderr) ) {
    $success = 0;
  }
  else {
    $success = 1;
  }

  #If the job was successful, then.....set the status to done.
  if ($success) {
    $qsout->update_job_status( $ref->{'id'}, 'DONE' );
  }
  else {
    $qsout->update_job_status( $ref->{'id'}, 'FAIL' );
  }
}
exit(0);
