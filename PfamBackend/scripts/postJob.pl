#!/usr/bin/perl

use strict;
use warnings;
use Data::Dumper;
use IO::File;
use Mail::Mailer;
use Getopt::Long;

use Bio::Pfam::WebServices::PfamQueue;

my($id, $debug, $tmpDir);
GetOptions( 'id=s'  => \$id,
			'tmp=s' => \$tmpDir,
			'debug' => \$debug);

my $qsout = Bio::Pfam::WebServices::PfamQueue->new("slow");

die unless($id);

my $ref = $qsout->get_job($id);


my $status = $ref->{'status'};
my ($error, $results);

if($status eq "RUN"){
	
	#Check that there is a result, even if it is empty
	if(!-e "$tmpDir/".$ref->{job_id}.".res" ){ 
		$error .= "No output produced\n";	
	}
	
	#If there is an error file and the output has zero size (or does not exisit), then somehting has gone wrong.
	if(-s "$tmpDir/".$ref->{job_id}.".err" && !-s "$tmpDir/".$ref->{job_id}.".res"){
		open( ERR, "$tmpDir/".$ref->{job_id}.".err") || die "Could not open $tmpDir/".$ref->{job_id}.".err: [$!]\n";
		while(<ERR>){
			$error .= $_;
		}
	}
	
	#If no errors have been detected, then this 
	if(!$error){
		my $FH;
		open($FH, "$tmpDir/".$ref->{job_id}.".res") 
			or $error .= "Could not open pfam_scan results file\n"; 
		if($FH){
			while(<$FH>){
				$results .= $_;
			}
		}
		#Update the job status to done!
		$qsout->update_job_status($ref->{id}, 'DONE');
		emailResults($ref->{email}, $results, $qsout->email);
		$qsout->update_job_stream($ref->{id}, 'stdout', $results);
		if($error){
			$qsout->update_job_stream($ref->{id}, 'stderr', $error);
		}
	}else{
	   $qsout->update_job_status($ref->{id}, 'FAIL');
   	   $qsout->update_job_stream($ref->{id}, 'stderr', $error);
	}
}elsif($status eq "PEND"){
	$error .= "Failed to submit the jobs\n";
	$qsout->update_job_status($ref->{id}, 'FAIL');
   	$qsout->update_job_stream($ref->{id}, 'stderr', $error);
}

$ref = $qsout->get_job($id);
$status = $ref->{'status'};

if($status eq "FAIL"){
	my $errorString = $ref->{'stderr'}."\n" if($ref->{'stderr'});
	$errorString .= $error if ($error);
	emailFail($ref->{email}, $errorString);
}



#Clean up files.....


sub emailResults {
	my ($email, $results, $emailHeader) = @_;
	print Dumper($emailHeader);
	my %header = ( 	To => $email,
					From => 'pfam-help@sanger.ac.uk',
					Subject => "Your pfam search results" );
	if(ref $emailHeader eq "HASH"){
		while (my ($key, $value) = each %$emailHeader){
			$header{$key} = $value;
		}
	}
	print Dumper( %header);
	my $mailer = Mail::Mailer->new;

	$mailer->open(\%header);
my $message = <<'__MESSAGE__';

Please find your Pfam search results below.

This is an experimental service, so if the results are not as expected, 
or if you have any comments about the use of this service, then please 
contact Pfam at: pfam@sanger.ac.uk.

For REALLY large jobs, you should probably download Pfam and run locally.
Please see the FAQ at http://pfam.sanger.ac.uk/help if you need 
help with this.

--------------

Output format is:
<seq id> <seq start> <seq end> <pfam acc> <hmm start> <hmm end> <bit score> <evalue> <pfam id>

Results:

$results  
 
__MESSAGE__
print $mailer $message;
$mailer->close;
}
exit(0);