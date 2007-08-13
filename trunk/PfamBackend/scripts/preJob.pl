#!/usr/bin/perl

use strict;
use warnings;
use Data::Dumper;
use IPC::Cmd qw(run);
use IO::File;
use Getopt::Long;

use Bio::Pfam::WebServices::PfamQueue;
my $qsout = Bio::Pfam::WebServices::PfamQueue->new("slow");

my($id, $debug, $tmpDir);
GetOptions( 'id=s'  => \$id,
			'tmp=s' => \$tmpDir,
			'debug' => \$debug);

die unless($id);
my $ref = $qsout->get_job($id);

my $error;
#Write the users sequence to file
open(FA, ">$tmpDir/".$ref->{job_id}.".fa") ||  ($error .= "Could not open file for writing:[$!].");
if($ref->{'stdin'} !~ /^>/){
	print FA ">UserSeq\n";
}
print FA $ref->{'stdin'}."\n";
close(FA)  || ($error .= "Could not close fasta file:[$!].");

if(!-s "$tmpDir/".$ref->{job_id}.".fa"){
	$error .= "Your fasta file has no size!\n";
}


if($error){
   $qsout->update_job_status($ref->{id}, 'FAIL');
   $qsout->update_job_stream($ref->{id}, 'stderr', $error);
}else{
   $qsout->update_job_status($ref->{id}, 'RUN');
}

exit(0);