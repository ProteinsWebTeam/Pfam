#!/usr/bin/perl

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
use IO::File;
use Mail::Mailer;
use Getopt::Long;
use Text::Wrap;

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
		emailResults($ref->{email}, $results, $qsout->email, $ref);
		$qsout->update_job_stream($ref->{id}, 'stdout', $results);
		if($error){
			$qsout->update_job_stream($ref->{id}, 'stderr', $error);
		}
	}else{
	   $qsout->update_job_status($ref->{id}, 'FAIL');
   	 $qsout->update_job_stream($ref->{id}, 'stderr', $error);
	}
}elsif($status eq "PEND"){
	$error .= "Failed to submit the job to backend farm\n";
	$qsout->update_job_status($ref->{id}, 'FAIL');
  $qsout->update_job_stream($ref->{id}, 'stderr', $error);
}

$ref = $qsout->get_job($id);
$status = $ref->{'status'};

if($status eq "FAIL"){
	my $errorString = $ref->{'stderr'}."\n" if($ref->{'stderr'});
	$errorString .= $error if ($error);
	emailFail($ref->{email}, $errorString, $qsout->email, $ref);
}



#Clean up files.....


sub emailResults {
	my ($email, $results, $emailHeader, $ref) = @_;
	my %header = ( 	To => $email,
					From => 'pfam-help@sanger.ac.uk',
					Subject => 'Your pfam search results for job '.$ref->{job_id} );
	if(ref $emailHeader eq "HASH"){
		while (my ($key, $value) = each %$emailHeader){
			$header{$key} = $value;
		}
	}
	my $mailer = Mail::Mailer->new;

	$mailer->open(\%header);
	
my $message = "Your job id was: ".$ref->{job_id}."\n";

if($ref->{'job_type'} eq 'batch'){ 
$message .= <<'__MESSAGE__';

Please find your Pfam search results below.

If you have any comments about the use of this service, please 
contact Pfam at: pfam-help@sanger.ac.uk.

For REALLY large jobs, you should download Pfam and run your jobs 
locally. Please see the FAQ at:

http://pfam.sanger.ac.uk/help?tab=helpFAQBlock#localSearch 

if you need help with this.

--------------

The following command was executed (default parameters not shown) :

__MESSAGE__
}elsif($ref->{'job_type'} eq 'dna'){

$message .= <<'__MESSAGE__';

Please find your Pfam DNA search results below.

If you have any comments about the use of this service, please 
contact Pfam at: pfam-help@sanger.ac.uk.

The DNA search tool uses a WU-BLAST prefilter and the Wise2 package to 
compare potential hits to the HMMs. You can see more information about 
Wise2 at the homepage, http://www.ebi.ac.uk/Tools/Wise2/index.html. If 
you have a large number of DNA searches, you can download the script 
used to run these searches from the Pfam CVS repository:

http://cvs.sanger.ac.uk/cgi-bin/viewcvs.cgi/PfamBackend/scripts/pfamDNASearch.pl?root=PfamWeb&view=log
 
--------------

The following command was executed (default parameters not shown) :

__MESSAGE__
}

$message .= $ref->{'command'}.' ';
if($ref->{'options'}){
  $message .= $ref->{'options'};
}

$message .= " your_input.fa\n";

$message .= <<'__MESSAGE__';

----------------

The top of your input file looked like this:

__MESSAGE__

my $seq = substr($ref->{stdin}, 0, 250);
$seq .= '...' if length( $ref->{stdin} ) >250;
$seq .= "\n";

$Text::Wrap::columns = 60;
$message .= wrap( '', '', $seq );

if($ref->{'job_type'} eq 'batch'){ 
$message .= <<'__MESSAGE__';

----------------

Output format is:
<seq id> <seq start> <seq end> <pfam acc> <hmm start> <hmm end> <alignment mode> <bit score> <evalue> <pfam id> <nested> <predicted_active_site_residues>
__MESSAGE__

}

$message .= <<'__MESSAGE__';

Results:

__MESSAGE__

$message .= $results."\n";

print $mailer $message;
$mailer->close;
}

sub emailFail{
  my ($email, $errorString, $emailHeader, $ref) = @_;
	
	my %header = ( To      => $email,
					       From    => 'pfam-help@sanger.ac.uk',
					       Subject => 'There was an ERROR running the job '.$ref->{job_id} );
	if(ref $emailHeader eq "HASH"){
		while (my ($key, $value) = each %$emailHeader){
			$header{$key} = $value;
		}
	}
	my $mailer = Mail::Mailer->new;
	$mailer->open(\%header);
	print $mailer $errorString;
  $mailer->close;
}
exit(0);
