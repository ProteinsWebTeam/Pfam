#!/usr/bin/perl

use strict;
use warnings;
use Data::Dumper;
use IO::File;
use Mail::Mailer;
use Bio::Pfam::WebServices::PfamQueue::lsf;
my $qsout = Bio::Pfam::WebServices::PfamQueue::lsf->new();

my $id = shift;

die unless($id);
my $debug = 1;

my $ref = $qsout->get_job($id);


my $status = $ref->status;
my ($error, $results);

if($status eq "RUN"){
	
	#Check that there is a result, even if it is empty
	if(!-e "/tmp/".$ref->{job_id}.".res" ){ 
		$error .= "No output produced\n";
	
	}
	
	#If there is an error file and the output has zero size (or does not exisit), then somehting has gone wrong.
	if(-s "/tmp/".$ref->{job_id}.".err" && !-s "/tmp/".$ref->{job_id}.".res"){
		open( ERR, "/tmp/".$ref->{job_id}.".err") || die "Could not open /tmp/".$ref->{job_id}.".err: [$!]\n";
		while(<ERR>){
			$error .= $_;
		}
	}
	
	#If no errors have been detected, then this 
	if(!$error){
		my $FH;
		open($FH, "/tmp/".$ref->{job_id}.".res") 
			or $error .= "Could not open pfam_scan results file\n"; 
		if($FH){
			while(<$FH>){
				$results .= $_;
			}
		}
		#Update the job status to done!
		$qsout->update_job_status($ref->{id}, 'DONE');
		emailResults($ref->{email}, $results);
		$qsout->update_job_stream($ref->{id}, 'stderr', $error);
	}else{
	   $qsout->update_job_status($ref->{id}, 'FAIL');
   	   $qsout->update_job_stream($ref->{id}, 'stderr', $error);
	}
}elsif($status eq "PEND"){
	$error .= "Failed to submit the jobs\n";
	$qsout->update_job_status($ref->{id}, 'FAIL');
   	$qsout->update_job_stream($ref->{id}, 'stderr', $error);
}

$status = $ref->status;

if($status eq "FAIL"){
	$errorString = $ref->{'stderr'}."\n" if($ref->{'stderr'});
	$errorString .= $error if ($error);
	emailFail($ref->{email}, $errorString);
}






sub emailResults {
	my ($email, $results) = @_;
	my %header = ( 	To => $email,
					From => 'pfam@sanger.ac.uk',
					Reply-To => 'pfam-help@sanger.ac.uk',
					Bcc => 'rdf@sanger.ac.uk, jt6@sanger.ac.uk',
					Subject => "Your pfam search results" );
	
	my $mailer = Mail::Mailer->new;
	my 
	
my $message = <<'__MESSAGE__';
To: $email
From: pfam@sanger.ac.uk
Bcc: rdf@sanger.ac.uk
Subject: Your pfam search results

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

my $sender = Email::Send->new({mailer => 'SMTP'});
$sender->mailer_args([Host => 'smtp.example.com']);
$sender->send($message);


}

sub email {

}
sub send

  my $sender = Email::Send->new({mailer => 'SMTP'});
  $sender->mailer_args([Host => 'smtp.example.com']);
  $sender->send($message);
	
	my $sendmail = "/usr/sbin/sendmail -t";
	my $reply_to = "Reply-to: pfam-help\@sanger.ac.uk\n";
	my $subject  = "Subject: Your Pfam scan results\n";
	my $content  = "Thanks for your submission.";
	my $to       = $email."\n";

	open (FILE, ">>$file") or die "Cannot open $file: $!";
	print $to,"\n";
	close(FILE); 

	my $send_to  = $email;

open(SENDMAIL, "|$sendmail") or die "Cannot open $sendmail: $!";
print SENDMAIL $reply_to;
print SENDMAIL $subject;
print SENDMAIL $send_to;
print SENDMAIL "Content-type: text/plain\n\n";
print SENDMAIL $content;
close(SENDMAIL);




#Write the users sequence to file
open(FA, ">/tmp/".$ref->{job_id}.".fa") ||  ($error .= "Could not open file for writing:[$!].");
print FA $ref->{'stdin'}."\n";
close(FA)  || ($error .= "Could not close fasta file:[$!].");

if($error){
   $qsout->update_job_status($ref->{id}, 'FAIL');
   $qsout->update_job_stream($ref->{id}, 'stderr', $error);
   next;
}else{
   $qsout->update_job_status($ref->{id}, 'RUN');
}