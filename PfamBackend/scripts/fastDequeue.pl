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
use IPC::Cmd qw(run);

# Our Modules
use Bio::Pfam::WebServices::PfamQueue;

#Switch on or off debugging
our $DEBUG = 0;

# Get a new queue stub of the type fast
my $qsout = Bio::Pfam::WebServices::PfamQueue->new("fast");
$qsout->daemonise unless($DEBUG);

while(1) {
  #Get a pending job
  my $ref   = $qsout->satisfy_pending_job();
  #$DEBUG && print Dumper($ref);
  
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
		
	   }elsif($ref->{'job_type'} eq "align" ){
        open(FA, ">".$qsout->tmpDir."/".$ref->{job_id}.".fa") ||  ($error .= "Could not open file for writing:[$!].");
	  	  print FA ">UserSeq\n" if ($ref->{'stdin'} !~ /^>/);
	   	  print FA $ref->{'stdin'}."\n";
		    close(FA)  || ($error .= "Could close fasta file:[$!].");
		   
		  if($error){
		    $qsout->update_job_status($ref->{id}, 'FAIL');
		    $qsout->update_job_stream($ref->{id}, 'stderr', $error);
		    next;
		  }
		  $cmd = $ref->{'command'}." -in ".$ref->{job_id}.".fa -tmp ".$qsout->tmpDir." -data ".$qsout->dataFileDir." ".$ref->{'options'};
		  
		  
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
