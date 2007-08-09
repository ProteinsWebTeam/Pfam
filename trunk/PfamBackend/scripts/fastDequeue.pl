#!/usr/bin/perl

use strict;
use warnings;
use Data::Dumper;
use IPC::Cmd qw(run);
use Bio::Pfam::WebServices::PfamQueue::pfamb;
my $qsout = Bio::Pfam::WebServices::PfamQueue::pfamb->new();

my $debug = 0;

while(1) {
  my $ref   = $qsout->satisfy_pending_job();
  $debug && print Dumper($ref);
  if($ref->{'id'}) {
  	my $error = 0;
    
		#Write the users sequence to file
		open(FA, ">/tmp/".$ref->{job_id}.".fa") ||  ($error .= "Could not open file for writing:[$!].");
		print FA ">UserSeq\n";
		print FA $ref->{'stdin'}."\n";
		close(FA)  || ($error .= "Could close fasta file:[$!].");
	   
	if($error){
			$qsout->update_job_status($ref->{id}, 'FAIL');
			$qsout->update_job_stream($ref->{id}, 'stderr', $error);
			next;
		}
		$debug && print STDERR "Executing id=$ref->{'id'}, command=$ref->{'command'}\n";
		
     my( $success, $error_code, $full_buf, $stdout_buf, $stderr_buf ) =
          run( command => $ref->{'command'}, verbose => 0 );

	  $debug &&  print STDERR Dumper($success, $error_code, $full_buf, $stdout_buf, $stderr_buf);
	
          
    if($success){
    	$qsout->update_job_status($ref->{'id'}, 'DONE');
    }else{
    	$qsout->update_job_status($ref->{'id'}, 'FAIL');
    }
    
    if(scalar(@$stdout_buf)){
    	$qsout->update_job_stream($ref->{id}, 'stdout', join "", @$stdout_buf);
    }     
    if(scalar(@$stderr_buf)){
    	$qsout->update_job_stream($ref->{id}, 'stderr', join "", @$stdout_buf);
    } 
    unlink("/tmp/".$ref->{job_id}.".fa") || warn "Could not remove tmp fasta file:[$!]\n";
  } else {
    sleep 5;
  }
}
