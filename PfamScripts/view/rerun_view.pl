#!/usr/bin/env perl

#Script to re-run view processes that have failed (eg due to deadlocks)
#Need to pass in a file of jobids, one per line 
#Script will set each job id to 'PEND' in the job_history table in the pfam_jobs database
#The view processes get run via the pfamJobDequeue.pl script

use strict;
use warnings;
use Bio::Pfam::PfamJobsDBManager;
use Bio::Pfam::Config;

my $list = shift @ARGV;
unless($list and -s $list) {
  die "Need to specify a file (containing a list of job_ids) on the command line\nE.g $0 <list>\n";
}


my $config = Bio::Pfam::Config->new;
my $pfamJobsDB = Bio::Pfam::PfamJobsDBManager->new( %{ $config->pfamjobs } );

open(L, $list) or die "Couldn't open fh to $list, $!";
while(<L>) {
  if(/(\S+)/) {
    my $job_id=$1;   # eg 5670A5E2-5745-11EA-9D79-66881E124DBD
    if($job_id =~ /(\S+)\//) {  
      chop $job_id;
    }
    my $row = $pfamJobsDB->getSchema->resultset('JobHistory')->find({ job_id => $job_id } );
    if($row) {
      print STDERR "Setting $job_id to 'PEND'\n";
      $row->update({ status => 'PEND',
                     opened => \'NOW()' });
    } 
    else {
      print STDERR "Couldn't find job id $job_id in the job_history table\n";
    }
  }
}



