#!/usr/bin/env perl 
#
# Script to fetch the RP sequence listing from PIR and update uniprot
# table with the relevant data.
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
my $store_dir = $config->localDbsLoc . '/RP';
unless ( -d $store_dir ) {
  mkdir($store_dir)
    or die "Could not make the directory '$store_dir' because: [$!]\n";
}

#Get cwd
my $cwd = getcwd;


#These are the different RP levels that are made available via PIR by default.
my @levels = qw(15 35 55 75);


#Copy pir data across
foreach my $level (@levels) {
  my $file = "rp-seqs-".$level.".fasta.gz";
  if(-e "$statusdir/copiedRP".$level) {
    $logger->debug("Already copied $file.gz");
  }
  else {
    $logger->debug("Copying $file from ".$config->uniprotPrivateLoc."/internal/rps_fromPIR/");
    copy($config->uniprotPrivateLoc."/internal/rps_fromPIR/$file", "$store_dir/$file") or $logger->logdie("Could not copy ".$config->uniprotPrivateLoc."/internal/rps_fromPIR/$file to $store_dir [$!]");
    touch("$statusdir/copiedRP$level");
  }
}

#The following update statements.  Note, that when a sequences appears in RP15, the
#most redundant version, then according to the RP definitions, it is in the
#less redundant versions.  Thus, we can reduce some of the updates. 
my %updateStatements = ( 15 =>  "UPDATE uniprot SET rp15=1, rp35=1, rp55=1, rp75=1 WHERE uniprot_acc = ?",
  35 =>  "UPDATE uniprot SET rp35=1, rp55=1, rp75=1 WHERE uniprot_acc = ?", 
  55 =>  "UPDATE uniprot SET rp55=1, rp75=1 WHERE uniprot_acc = ?",
  75 =>  "UPDATE uniprot SET rp75=1 WHERE uniprot_acc=?");

#Set all flags to be zero - this is possibly a slight waste as they should all
#be zero, but only takes <1 hour to run this double check.
if(-e "$statusdir/resetUniprotRP"){
  $logger->info("Already reset uniprot RP flags.");
}else{
  $logger->info("Resetting uniprot RP flag.");
  $dbh->do("UPDATE uniprot SET rp15=0, rp35=0, rp55=0, rp75=0");
  touch("$statusdir/resetUniProtRP");
}

my %seen;
foreach my $l (@levels){
  #Have we already processed this level?
  if( -e "$statusdir/setUniprotRP$l"){
    $logger->info("Already set uniprot RP flags for $l.");
  }else{
    chdir($store_dir) or $logger->logdie("Couldn't chdir into $store_dir, $!");
    $logger->info("Going to set uniprot RP flags for $l.");
    #Prepare the appropriate update statement.
    my $sth = $dbh->prepare($updateStatements{$l});

    #Define the filename
    my $file = "rp-seqs-$l.fasta.gz";


    #Read in the list of sequence accessions, which currently do not contian the version.
    open(F, "gunzip -c $store_dir/$file |") or $logger->logdie("Could not open $file for reading :[$!]");
    #Perform commits in blocks of 2000.
    $dbh->begin_work;
    my $c = 0;
    while(<F>){
      if(/^>(\S+)/) {
        my $acc=$1;
        unless(exists($seen{$acc})){
          $sth->execute($acc);
          $c++;
          $seen{$acc}++; 
        }
        if($c == 2000){
          $dbh->commit;
          $dbh->begin_work;
          $c = 0;  
        }
      }
    }
    close(F);
    $dbh->commit;
    chdir($cwd) or $logger->logdie("Couldn't chdir into $cwd, $!");
    touch("$statusdir/setUniprotRP$l");
  }
}


sub help {

  print <<EOF;

Usage: $0 -statusdir <release_status_dir>


Function: Script to fetch the RP sequence listing from uniprot and update the
uniprot table with the relevant data.  Columns rp15, rp35, rp55 and rp75 will 
be set to 1 if the sequence accession matches.


EOF

  exit(1); 
}
