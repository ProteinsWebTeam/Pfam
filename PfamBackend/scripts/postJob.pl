#!/usr/local/bin/perl

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

#------------------------------------------------------------------------------
#
# The purpose of this script is to mail asynchronous results to users.  As there
# are a growing number of jobs, this script is becoming a little untidy as more 
# jobs are added.  
#
# The pfam_backend.conf controls who the mails are sent from as well as any
# additional Bcc's so that you can monitor who is spamming the servers.
#
# 


use strict;
use warnings;
use Data::Dumper;
use IO::File;
use Mail::Mailer;
use Getopt::Long;
use Text::Wrap;

use Bio::Pfam::WebServices::PfamQueue;

my ( $id, $debug, $tmpDir );
GetOptions( 'id=s'  => \$id,
            'tmp=s' => \$tmpDir,
            'debug' => \$debug );

my $qsout = Bio::Pfam::WebServices::PfamQueue->new('slow');

die 'No job ID supplied' unless $id;

my $ref = $qsout->get_job($id);

my $email_address; 

if ( $ref->{job_type} eq 'rfam_batch' ) {
  $email_address = $qsout->rfamEmail;	
}
else {
  $email_address = $qsout->email;
}

print Dumper($email_address);
my $status = $ref->{status};
my ( $error, $results );

if ( $status eq 'RUN' ) {

  my $err_file = $tmpDir . '/' . $ref->{job_id} . '.err';
  my $res_file = $tmpDir . '/' . $ref->{job_id} . '.res';

  # check that there is a result, even if it is empty
  if ( ! -e $res_file ) { 
    $error .= "No output produced.\n";  
  }

  # if there is an error file and the output has zero size (or does not exisit), 
  # then something has gone wrong. Read in the contents of the error file
  if (   -s $err_file and 
       ! -s $res_file ) { 
    open ( ERR, $err_file ) 
      or die "Could not open '$err_file': [$!]\n";
    while ( <ERR> ) { $error .= $_; }
  }
  
  # if no errors have been detected...
  if ( ! $error ) {
    if ( open ( RES, $res_file ) ) {
      while ( <RES> ) { $results .= $_; }
    }
    else {
      $error .= "Could not open results file.\n"; 
    }

    # update the job status to done
    $qsout->update_job_status( $ref->{id}, 'DONE' );

    # mail the results out
    emailResults( $ref->{email}, 
                  $results, 
                  $email_address, 
                  $ref );

    # update the job_stream table with the results and any error messages
    $qsout->update_job_stream( $ref->{id}, 'stdout', $results );
    $qsout->update_job_stream( $ref->{id}, 'stderr', $error ) if $error;
  }
  else {
    # set the status to FAIL and put the error messages into the table
    $qsout->update_job_status( $ref->{id}, 'FAIL' );
    $qsout->update_job_stream( $ref->{id}, 'stderr', $error );
  }
}
elsif ( $status eq 'PEND' ) {
  $error .= "Failed to submit the job to backend farm\n";
  $qsout->update_job_status( $ref->{id}, 'FAIL' );
  $qsout->update_job_stream( $ref->{id}, 'stderr', $error );
}

$ref = $qsout->get_job( $id );
$status = $ref->{status};

if ( $status eq 'FAIL' ) {
  my $errorString = $ref->{stderr} . "\n" if $ref->{stderr};
  emailFail( $ref->{email}, 
             $errorString, 
             $email_address, 
             $ref );
}

# Clean up files.....

exit;

#-------------------------------------------------------------------------------
#- functions -------------------------------------------------------------------
#-------------------------------------------------------------------------------

# returns results of a successfully completed job to the submitter

sub emailResults {
  my ( $email, $results, $emailHeader, $ref ) = @_;

  my $source;
  if($ref->{job_type} eq 'rfam_batch'){
    $source = 'Rfam';	
  }else{
    $source = 'Pfam';	
  }


  # build email headers
  my %header = ( To      => $email,
                 Subject => 'Your '.$source.' search results for job ' . $ref->{job_id} );

  if ( ref $emailHeader eq 'HASH' ) {
    while (my ( $key, $value ) = each %$emailHeader ) {
      $header{$key} = $value;
    }
  }

  # start an email...
  my $mailer = Mail::Mailer->new;

  $mailer->open( \%header );
  
  my $message = 'Your job id was: ' . $ref->{job_id} . "\n";

  #----------------------------------------
  
  # use a different search description for batch (protein sequence) or DNA 
  # sequence jobs

  if ( $ref->{job_type} eq 'batch' ) { 
  
    $message .= <<'__MESSAGE__';

Please find your Pfam search results below.

If you have any comments or questions about the use of this service, 
please contact Pfam at: pfam-help@sanger.ac.uk

For REALLY large jobs, you should download Pfam and run your jobs 
locally. Please see the FAQ at:

http://pfam.sanger.ac.uk/help?tab=helpFAQBlock#localSearch 

if you need help with this.
__MESSAGE__

  }
  elsif ( $ref->{job_type} eq 'dna' ) {

    $message .= <<'__MESSAGE__';

Please find your Pfam DNA search results below.

If you have any comments or questions about the use of this service, 
please contact Pfam at: pfam-help@sanger.ac.uk

The DNA search tool uses a BLASTx prefilter and the Wise2 package to 
compare potential hits to the HMMs. You can see more information about 
Wise2 at:

http://www.ebi.ac.uk/Tools/Wise2/index.html

If you have a large number of DNA searches, you can download the script 
used to run these searches from the Pfam CVS repository:

http://cvs.sanger.ac.uk/cgi-bin/viewcvs.cgi/PfamBackend/scripts/pfamDNASearch.pl?root=PfamWeb 
__MESSAGE__

  }elsif ($ref->{job_type} eq 'rfam_batch' ){
 	$message .= <<'__MESSAGE__';

Please find your rfamScanLite results below.

If you have any comments or questions about the use of this service, 
please contact Rfam at: rfam-help@sanger.ac.uk
__MESSAGE__
  }

  #----------------------------------------

  # executed command

  $message .= <<'__MESSAGE__';

--------------

The following command was executed (default parameters not shown) :

__MESSAGE__
  $message .= $ref->{command} . ' ';
  $message .= $ref->{options} if $ref->{options};  
  $message .= " your_input.fa\n";

  #----------------------------------------
  
  # search sequence

  $message .= <<'__MESSAGE__';

----------------

The top of your input file looked like this:

__MESSAGE__

  my $seq = substr( $ref->{stdin}, 0, 250 );

  $Text::Wrap::columns = 60;
  $message .= wrap( '', '', $seq );

  $message .= '...' if length( $ref->{stdin} ) > 250;
  $message .= "\n";

  #----------------------------------------
  
  # and finally, the results...

  if ( not defined $results or $results =~ m/^\s*$/ ) {

    # no results...

    $message .= <<'__MESSAGE__';

----------------

Your job ran successfully but we found no matches to your sequence. 

__MESSAGE__

  }
  else {

    # we got some results

    if ( $ref->{job_type} eq 'batch' ) { 
      $message .= <<'__MESSAGE__';

----------------

__MESSAGE__

    }elsif($ref->{job_type} eq 'rfam_batch'){
      $message .= <<'__MESSAGE__';

----------------

The output format is:
<rfam acc> <rfam id> <seq id> <seq start> <seq end> <strand> <score>
__MESSAGE__
      
    }

    $message .= <<'__MESSAGE__';

Results:

__MESSAGE__

    $message .= $results . "\n";

  }

  #----------------------------------------
  
  # close the mail and send it

  print $mailer $message;
  $mailer->close;
}

#-------------------------------------------------------------------------------

# returns an error message to the submitter

sub emailFail{
  my ( $email, $errorString, $emailHeader, $ref ) = @_;
  
  my %header = ( To      => $email,
                 From    => 'pfam-help@sanger.ac.uk',
                 Subject => 'There was an ERROR running the job ' . $ref->{job_id} );

  if ( ref $emailHeader eq 'HASH' ) {
    while ( my ( $key, $value ) = each %$emailHeader ) {
      $header{$key} = $value;
    }
  }

  my $mailer = Mail::Mailer->new;
  $mailer->open( \%header );

  my $message .= <<'__MESSAGE__';

We encountered the following errors when running the job:

__MESSAGE__

  $message .= $errorString;

  print $mailer $message;
  $mailer->close;
}
