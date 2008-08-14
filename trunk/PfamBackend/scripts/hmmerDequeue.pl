#!/usr/bin/perl
#
# Authors: Rob Finn & John Tate 
#
# This script is designed to take jobs entered into the web_user database
# and run them.  This dequeuer will only run interactive hmmer jobs. 
# Currently, there is no down weighting of users who submit lots of jobs.  
# The results of fast jobs are entered in to the database.
#
# This only runs pfam_scan.pl which is a wrapper around hmmer and deals with
# some of the post processing.  We run pfam_scan.pl as this give the users
# exactly the same results whether they run the batch form of the sequence search.

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
use IPC::Cmd qw(run);
use Bio::Pfam::WebServices::PfamQueue;

our $DEBUG = 0;

#Get a queue stub of type hmmer
my $qsout = Bio::Pfam::WebServices::PfamQueue->new("hmmer");
$qsout->daemonise unless($DEBUG);
#An infinite loop to keep submitting jobs.
while(1) {
  #See if there are any jobs in the queue
  my $ref   = $qsout->satisfy_pending_job();
  $DEBUG && print Dumper($ref);
  
  unless($ref->{'id'}){
  	sleep 2;
  }else{
  	my $error = 0;
    my $cmd;
    
	#Write the users sequence to file
	open(FA, ">".$qsout->tmpDir."/".$ref->{job_id}.".fa") ||  ($error .= "Could not open file for writing:[$!].");
	print FA ">UserSeq\n";
	print FA $ref->{'stdin'}."\n";
	close(FA)  || ($error .= "Could close fasta file:[$!].");
	
	
	$cmd = $ref->{'command'};
	if($qsout->pvm){
		$cmd .= " -pvm"
	}else{
		$cmd .= " -cpu ".$qsout->cpus;
	}
	$cmd .= " -align -d ".$qsout->dataFileDir;
  $cmd .= " -as -as_dir /home/pfamweb/AS";
	$cmd .= " ".$ref->{'options'};
	$cmd .= " ".$qsout->tmpDir."/".$ref->{job_id}.".fa";

	   
	if($error){
	  $qsout->update_job_status($ref->{id}, 'FAIL');
	  $qsout->update_job_stream($ref->{id}, 'stderr', $error);
	  next;
	}
	$DEBUG && print STDERR "Executing id=$ref->{'id'}, command=$cmd\n";
		
    my( $success, $error_code, $full_buf, $stdout_buf, $stderr_buf ) =
          run( command => $cmd, verbose => 0 );

	$DEBUG &&  print STDERR Dumper($success, $error_code, $full_buf, $stdout_buf, $stderr_buf);
	      
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
    unlink($qsout->tmpDir."/".$ref->{job_id}.".fa") || warn "Could not remove tmp fasta file:[$!]\n";
  } 
}
