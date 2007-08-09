#!/usr/bin/perl

use strict;
use warnings;
use Data::Dumper;
use IPC::Cmd qw(run);
use IO::File;
use Bio::Pfam::WebServices::PfamQueue::lsf;
my $qsout = Bio::Pfam::WebServices::PfamQueue::lsf->new();

my $debug = 1;

while(1) {
  my $ref   = $qsout->satisfy_pending_job();
  $debug && print Dumper($ref);
  if($ref->{'id'}) {
  	my $error = 0;
    
	$debug && print STDERR "Submitting id=$ref->{'id'}, command=$ref->{'command'}\n";
			
	#Now for the lsf bit
	if($ref->{'command'} =~ /pfam_scan.pl|pfamdna_blast.pl/) {
		my $fh = IO::File->new;
		$fh->open( "| bsub -q pfam_slow -sp 1 ");
		$fh->print( "preLSFJob.pl ".$ref->{id}."\n");
		$fh->print( $ref->{'command'}." > /tmp/".$ref->{job_id}.".res 2> /tmp/".$ref->{job_id}.".err\n");
		$fh->print( "postLSFJob.pl ".$ref->{id}."\n");
		$fh->close;
	}else{
		$error .= "unrecognised command line, commoand=$ref->{'command'}\n";
	}
	
	if($error){		
		$qsout->update_job_status($ref->{id}, 'FAIL');
		$qsout->update_job_stream($ref->{id}, 'stderr', $error);
		next;
	}
    
  } else {
   sleep 5;
  }
}
