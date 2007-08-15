#!/usr/bin/perl
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
#

# Standard Modules
use strict;
use warnings;
use Data::Dumper;
use IPC::Cmd qw(run);

# Our Modules
use Bio::Pfam::WebServices::PfamQueue;

#Switch on or off debugging
our $DEBUG = 0;

# Get a new queue stub of the type fast
my $qsout = Bio::Pfam::WebServices::PfamQueue->new("fast");

while(1) {
  #Get a pending job
  my $ref   = $qsout->satisfy_pending_job();
  $DEBUG && print Dumper($ref);
  
  unless($ref->{'id'}){
	#No pending jobs
	sleep 2;
  }else {
  	my $error = 0;
	my $cmd;
  	if($ref->{'job_type'} eq "pfamb"){
		#Write the users sequence to file
		open(FA, ">".$qsout->tmpDir."/".$ref->{job_id}.".fa") ||  ($error .= "Could not open file for writing:[$!].");
		print FA ">UserSeq\n";
		print FA $ref->{'stdin'}."\n";
		close(FA)  || ($error .= "Could close fasta file:[$!].");
		   
		if($error){
		  $qsout->update_job_status($ref->{id}, 'FAIL');
		  $qsout->update_job_stream($ref->{id}, 'stderr', $error);
		  next;
		}
		
		#There are no options from the user for these jobs at the moment.
		#Append on the file locations of Pfam-B.fasta and tmp file
		$cmd = $ref->{'command'}." ".$qsout->dataFileDir."/Pfam-B.fasta ".$qsout->tmpDir.
				"/".$ref->{job_id}.".fa -cpus ".$qsout->cpus." -gapE=2000 -T=12";
		
	}
	
	$DEBUG && print STDERR "Executing id=$ref->{'id'}, command=$cmd\n";
	
	#Run the executable via IPC::Cmd
    my( $success, $error_code, $full_buf, $stdout_buf, $stderr_buf ) =
    	run( command => $cmd, verbose => 0 );

	$DEBUG &&  print STDERR Dumper($success, $error_code, $full_buf, $stdout_buf, $stderr_buf);
          
    
    #Put anything we have captured in stdout and stderr back in the database
    if(scalar(@$stdout_buf)){
    	$qsout->update_job_stream($ref->{id}, 'stdout', join "", @$stdout_buf);
    }     
    if(scalar(@$stderr_buf)){
    	$qsout->update_job_stream($ref->{id}, 'stderr', join "", @$stdout_buf);
    }

    if($success){
       $qsout->update_job_status($ref->{'id'}, 'DONE');
    }else{
       $qsout->update_job_status($ref->{'id'}, 'FAIL');
    }

     #Clean up any output files
    if(-e $qsout->tmpDir."/".$ref->{job_id}.".fa"){
      unlink($qsout->tmpDir."/".$ref->{job_id}.".fa") || warn "Could not remove tmp fasta file:[$!]\n";
 	}
  }
}
