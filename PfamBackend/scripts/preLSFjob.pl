#!/usr/bin/perl

use strict;
use warnings;
use Data::Dumper;
use IPC::Cmd qw(run);
use IO::File;
use Bio::Pfam::WebServices::PfamQueue::lsf;
my $qsout = Bio::Pfam::WebServices::PfamQueue::lsf->new();

my $id = shift;

die unless($id);
my $debug = 1;

my $ref = $qsout->get_job($id);

#Write the users sequence to file
open(FA, ">/tmp/".$ref->{job_id}.".fa") ||  ($error .= "Could not open file for writing:[$!].");
print FA $ref->{'stdin'}."\n";
close(FA)  || ($error .= "Could not close fasta file:[$!].");

if(!-s "tmp/".$ref->{job_id}.".fa"){
	$error .= "Your fasta file has now size!\n";
}


if($error){
   $qsout->update_job_status($ref->{id}, 'FAIL');
   $qsout->update_job_stream($ref->{id}, 'stderr', $error);
   next;
}else{
   $qsout->update_job_status($ref->{id}, 'RUN');
}

exit(0);