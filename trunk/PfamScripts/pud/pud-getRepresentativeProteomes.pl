#!/usr/bin/env perl 
#
# Script to fetch the RP sequence listing from PIR and update pfamseq
# with the relevant data.
#
use strict;
use warnings;
use LWP::UserAgent;
use HTTP::Date;
use Getopt::Long;
use Log::Log4perl qw(:easy);
use File::Touch;
use File::Copy;
use Cwd;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my ( $statusdir );

#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

&GetOptions(
  "statusdir=s"  => \$statusdir,
) or $logger->logdie("Invalid option!\n");

unless ( $statusdir and -e $statusdir ) {
  help();
}

#Database connection stuff
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh    = $pfamDB->getSchema->storage->dbh;

#does the RP local database directory exist? If not make one
unless ( -d $config->localDbsLoc . '/RP' ) {
  mkdir( $config->localDbsLoc . '/RP' )
    or die "Could not make the directory "
    . $config->localDbsLoc
    . "/RP because: [$!]\n";
}

#Move to the RP local database directory;
my $pwd = getcwd;
my $store_dir = $config->localDbsLoc."/RP";
chdir($store_dir) or $logger->logdie("Failed to change into $store_dir");


#These are the different RP levels that are made available via PIR by default.
my @levels = qw(15 35 55 75);

#The following update statements.  Note, that when a sequences appears in RP15, the
#most redundant version, then according to the RP definitions, it is in the the 
#less redundant versions.  Thus, we can reduce some of the updates. 
my %updateStatements = ( 15 =>  "UPDATE pfamseq SET rp15=1, rp35=1, rp55=1, rp75=1 WHERE pfamseq_acc = ?",
                         35 =>  "UPDATE pfamseq SET rp35=1, rp55=1, rp75=1 WHERE pfamseq_acc = ?", 
                         55 =>  "UPDATE pfamseq SET rp55=1, rp75=1 WHERE pfamseq_acc = ?",
                         75 =>  "UPDATE pfamseq SET rp75=1 WHERE pfamseq_acc=?");

#Set all flags to be zero - this is possibly a slight waste as they should all
#be zero, but only takes about 15 minutes to run this double check.
if(-e "$statusdir/restPfamseqRP"){
  $logger->info("Already reset pfamseq RP flags.");
}else{
  $logger->info("Resetting pfamseq RP flag.");
  $dbh->do("UPDATE pfamseq SET rp15=0, rp35=0, rp55=0, rp75=0");
  touch("$statusdir/restPfamseqRP");
}

my $agent = LWP::UserAgent->new;
$agent->env_proxy;

#Build up the path to something that looks like this.
#http://pir.georgetown.edu/rps/data/rp_seq_blast_db/current/15/rp-seqs-15.txt
my $baseURL = "http://pir.georgetown.edu/rps/data/rp_seq_blast_db/current";

my %seen;
foreach my $l (@levels){
  #Have we already processed this level?
  if( -e "$statusdir/setPfamseqRP$l"){
    $logger->info("Already set pfamseq RP flags for $l.");
  }else{
    
    $logger->info("Going to set pfamseq RP flags for $l.");
    #Prepare the appropriate update statement.
    my $sth = $dbh->prepare($updateStatements{$l});
    
    #Finish off the URL
    my $url = $baseURL."/$l/rp-seqs-$l.txt";
    my $file = "rp-seqs-$l.txt";
    
    #Download the file
    my $response = $agent->mirror($url, $file);
    if ($response->is_success) {
      my $date = sprintf("%4d-%02d-%02d", HTTP::Date::parse_date($response->header('Last-Modified')));
      $logger->info("File $file: downloaded PIR $file ($date)");
    } elsif ($response->code == HTTP::Status::RC_NOT_MODIFIED) {
      $logger->info("File $file: up-to-date\n");
    } else {
      $logger->logdie( 'Failed, got ' . $response->status_line .
        ' for ' . $response->request->uri );
    }
    
    #Read in the list of sequence accessions, which currently do not contian the version.
    open(F, "<", $file) or $logger->logdie("Could not open $file for reading :[$!]");
    #Perform commits in blocks of 2000.
    $dbh->begin_work;
    my $c = 0;
    while(<F>){
      chomp;
      unless(exists($seen{$_})){
        $sth->execute($_);
        $c++;
        $seen{$_}++; 
      }
      if($c == 2000){
        $dbh->commit;
        $dbh->begin_work;
        $c = 0;  
      }
    }
    close(F);
    $dbh->commit;
    touch("$statusdir/setPfamseqRP$l");
  }
}


sub help {

print <<EOF;

Usage: $0 -statusdir <release_status_dir>


Function: Script to fetch the RP sequence listing from PIR and update pfamseq 
with the relevant data.  Columns rp15, rp35, rp55 and rp75 will be set to 1
if the sequence accession matches. PIR releases are linked to UniProt, so 
they should be in sync.....


EOF

 exit(1); 
}
