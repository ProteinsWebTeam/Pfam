#!/usr/bin/env perl
#
# Authors: Rob Finn & John Tate
#
# This script is designed to take jobs entered into the web_user database
# and run them.  This dequeuer will only run really fast jobs that take <5 secs.
# This is the does not run singe sequecne hmmer jobs.  Currently, there is
# no down weighting of users who submit lots of jobs.  The results of fast jobs
# are entered in to the database.
#
# The executables that this current runs are
# wu-blastp a query sequence against the pfamb fasta file.

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

# Standard Modules
use strict;
use warnings;
use Data::Dumper;
use Data::Printer;
use IPC::Open3;
use IO::Handle;
use IPC::Cmd qw(run);

# Our Modules
use Bio::Pfam::WebServices::PfamQueue;

#Switch on or off debugging
our $DEBUG = defined( $ENV{DEBUG} ) ? $ENV{DEBUG} : 0;

my $opts = {};

if ( defined $ENV{PIDFILE} ) {
  $opts->{pidfile} = $ENV{PIDFILE};
}

# Get a new queue stub of the type fast
my $qsout = Bio::Pfam::WebServices::PfamQueue->new( "fast", $opts );
$qsout->daemonise unless ($DEBUG);

if ( $DEBUG && $ENV{PIDFILE} ) {
  open my $pid, '>', $ENV{PIDFILE}
    or die "Couldn't open pidfile: $!";
  print $pid $$;
  close $pid;
}

JOB: while (1) {

  #Get a pending job
  my $ref = $qsout->satisfy_pending_job();

  #$DEBUG && print Dumper($ref);

  unless ( $ref->{'id'} ) {
    #No pending jobs
    sleep 1;
  }
  else {
    my $error = 0;
    my $cmd;
    if ( $ref->{'job_type'} eq "pfamb" ) {

      #Write the users sequence to file
      open( FA, ">" . $qsout->tmpDir . "/" . $ref->{job_id} . ".fa" )
        || ( $error .= "Could not open file for writing:[$!]." );
      print FA ">UserSeq\n";
      print FA $ref->{'stdin'} . "\n";
      close(FA) || ( $error .= "Could close fasta file:[$!]." );

      if ($error) {
        $qsout->update_job_status( $ref->{id}, 'FAIL' );
        $qsout->update_job_stream( $ref->{id}, 'stderr', $error );
        next;
      }

      #There are no options from the user for these jobs at the moment.
      #Append on the file locations of Pfam-B.fasta and tmp file
      $cmd =
          $ref->{'command'} . " "
        . $qsout->dataFileDir
        . "/Pfam-B.fasta "
        . $qsout->tmpDir . "/"
        . $ref->{job_id}
        . ".fa -cpus "
        . $qsout->cpus
        . " -gapE=2000 -T=12";
      executeCmd( $cmd, $ref, $qsout );
    }
    elsif ( $ref->{'job_type'} eq "align" ) {
      open( FA, ">" . $qsout->tmpDir . "/" . $ref->{job_id} . ".fa" )
        || ( $error .= "Could not open file for writing:[$!]." );
      print FA ">UserSeq\n" if ( $ref->{'stdin'} !~ /^>/ );
      print FA $ref->{'stdin'} . "\n";
      close(FA) || ( $error .= "Could close fasta file:[$!]." );

      if ($error) {
        $qsout->update_job_status( $ref->{id}, 'FAIL' );
        $qsout->update_job_stream( $ref->{id}, 'stderr', $error );
        next;
      }
      $cmd =
          $ref->{'command'} . " -in "
        . $ref->{job_id}
        . ".fa -tmp "
        . $qsout->tmpDir
        . " -data "
        . $qsout->dataFileDir . " "
        . $ref->{'options'};
      executeCmd( $cmd, $ref, $qsout );
    }
    elsif ( $ref->{job_type} eq 'rfam' ) {

#The most common job at the time of writing. This searches a single sequence against the library of CMs.
#Modified to you cmscan directly.

      #Get the users sequence from the database.
      #NOTE: there are currently
      my $input;
      $input = ">UserSeq\n" if $ref->{stdin} !~ /^>/;
      $input .= $ref->{stdin} . "\n";


      #The command we want to run.
      my $cmd =
          "cmscan --cut_ga --notextw --cpu "
        . $qsout->rfcpus
        . " --FZ 5 --nohmmonly "
        . $qsout->rfamDataFileDir
        . "/Rfam.cm.1_1 -";
      $DEBUG && print "Going to run:$cmd\n";

    executeCmd3( $cmd, $ref, $qsout, $input);
  }
    elsif ( $ref->{job_type} eq 'rfalign' ) {

      #From the species sunburst of tree, build a subset of the alignment.
      #In the past, this has been done by realigning the sequences to the model.
      #EPN noted that this is possibly in efficient for large alignments. After
      #some test established that most queries would return in 1-2 secs.
      #New procedure is to fetch the full alignment, then use an easel app to
      #grab out the sequecnes.  Finally, remove all gab columns.

     #The list of sequences that want to keep. Currently, the server is fetching
     #these sequences. We only need the accession.
      my $listfile = $qsout->tmpDir . "/" . $ref->{job_id} . ".list";
      open( LIST, ">" . $listfile )
        || ( $error .= "Could not open file for writing:[$!]." );
      foreach my $l ( split /\n/, $ref->{stdin} ) {
        if ( $l =~ /^\>(\S+)/ ) {
          print LIST $1 . "\n";
        }
      }
      close(LIST) || ( $error .= "Could close list file:[$!]." );
      
      if ($error) {
        $qsout->update_job_status( $ref->{id}, 'FAIL' );
        $qsout->update_job_stream( $ref->{id}, 'stderr', $error );
        next;
      }
      
      #The series of commands we want to run.
      $cmd =
          'esl-afetch '
        . $qsout->rfamDataFileDir
        . "/Rfam.full "
        . $ref->{options}
        . " | esl-alimanip --informat stockholm --seq-k $listfile - | esl-alimask --informat stockholm -g --gapthresh 0.9999999 -";
      
      $DEBUG && print "Going to run:$cmd\n";

      executeCmd3( $cmd, $ref, $qsout );

      #Finally, remove the list file
      if ( -e $qsout->tmpDir . "/" . $ref->{job_id} . ".list" ) {
        unlink( $qsout->tmpDir . "/" . $ref->{job_id} . ".list" )
          || warn "Could not remove tmp list file:[$!]\n";
      }
    }
  }
}

sub executeCmd {
  my ( $cmd, $ref, $qsout ) = @_;

  $DEBUG && print STDERR "Executing id=$ref->{'id'}, command=$cmd\n";

  #Run the executable via IPC::Cmd
  my ( $success, $error_code, $full_buf, $stdout_buf, $stderr_buf ) =
    run( command => $cmd, verbose => 0 );

  $DEBUG
    && print STDERR Dumper( $success, $error_code, $full_buf, $stdout_buf,
    $stderr_buf );

  #Put anything we have captured in stdout and stderr back in the database
  if ( scalar(@$stdout_buf) ) {
    $qsout->update_job_stream( $ref->{id}, 'stdout', join "", @$stdout_buf );
  }
  if ( scalar(@$stderr_buf) ) {
    $qsout->update_job_stream( $ref->{id}, 'stderr', join "", @$stdout_buf );
  }

  if ($success) {
    $qsout->update_job_status( $ref->{'id'}, 'DONE' );
  }
  else {
    $qsout->update_job_status( $ref->{'id'}, 'FAIL' );
  }

  #Clean up any output files
  if ( -e $qsout->tmpDir . "/" . $ref->{job_id} . ".fa" ) {
    unlink( $qsout->tmpDir . "/" . $ref->{job_id} . ".fa" )
      || warn "Could not remove tmp fasta file:[$!]\n";
  }
}

sub executeCmd3 {
    my( $cmd, $ref, $qsout, $input) = @_;
    #We are going to use IPC::Open3 to submit the search job. This allows us to pass the sequence in via STDIN.
#NOTE: HMMER/Infernal does this via the - field in the command line.
    
     my ( $reader, $writer, $err ) =
        ( IO::Handle->new, IO::Handle->new, IO::Handle->new );
  
    $DEBUG && print "Going to run:$cmd\n";

      #Then the joys of open3 set up
      my $pid = undef;
      my $child_exit_status;
      eval {
        $pid = open3($writer, $reader, $err, $cmd);

      };
      if ($@) {
        die "open3 failed: $!\n$@\n";
      }
      
      if(defined($input)){
        print $writer $input;
      }
      close $writer;
    
      #Put anything we have captured in stdout and stderr back in the database
      my @stdout = $reader->getlines;
      if(scalar(@stdout)){
        $qsout->update_job_stream($ref->{id}, 'stdout', join "", @stdout);
      }
      close($reader);
  
      my @stderr = $err->getlines;
      if(scalar(@stderr)){
        print STDERR join "", @stderr;
        $qsout->update_job_stream($ref->{id}, 'stderr', join "", @stderr);
      }
      close($err);

      #Failing to do this can result in an accumulation of defunct or "zombie" processes.
      #Only do it when we have closed all of the IO::Handles
      waitpid( $pid, 0 );

      #Determine the exist status of the child process.
      #We can also check stderr.  Anything in either will indicate some sort of issue. 
      $child_exit_status = $? >> 8;
      my $success = 0;
      if($child_exit_status or scalar(@stderr)){ 
        $success=0;
      }else{
        $success=1;
      }
      
      #If the job was successful, then.....set the status to done.
      if ($success) {
        $qsout->update_job_status( $ref->{'id'}, 'DONE' );
      }
      else {
        $qsout->update_job_status( $ref->{'id'}, 'FAIL' );
      }
}
