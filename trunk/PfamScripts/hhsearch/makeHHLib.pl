#!/usr/local/bin/perl

use strict;
use warnings;
use Log::Log4perl qw(:easy);
use Cwd;
use File::Temp qw(tempdir);
use Storable qw(nfreeze);
use Data::Dumper;

print STDERR $Storable::VERSION . "\n";
use Bio::Pfam::Pfetch;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::ViewProcess;


Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
unless ($pfamDB) {
  Bio::Pfam::ViewProcess::mailPfam(
    "View process failed as we could not connect to pfamlive");
}

$logger->debug("Got pfamlive database connection");
my $dbh = $pfamDB->getSchema->storage->dbh;

my $jobDB = Bio::Pfam::PfamJobsDBManager->new( %{ $config->pfamjobs } );
unless ($jobDB) {
  Bio::Pfam::ViewProcess::mailPfam( "Failed to run view process",
    "Could not get connection to the pfam_jobs database" );
}

my @pfamAFams;# = $pfamDB->getSchema->resultset('Pfama')->search({});
my $tempDir = "/tmp";

#-------------------------------------------------------------------------------
#Run HHsearch
my @modFams;
foreach my $fam (@pfamAFams) {
  
  #Remove old hmms  
  if ( -e ( $config->hhsearchLibDir . "/" . $fam->pfama_acc.".hmm" ) ) {
    unlink( $config->hhsearchLibDir . "/" . $fam->pfama_acc.".hmm" );
  }

  #Get the SEED alignment and construct a HMM using this.
  my $seed = $pfamDB->getSchema->resultset('AlignmentsAndTrees')->find(
    {
      auto_pfama => $fam->auto_pfama,
      type       => 'seed'
    }
  );
  $logger->debug("Fetching seed");
  open( S, '>'.$tempDir . "/" . $fam->pfama_acc . ".seed" ) or $logger->logdie("");
  print S Compress::Zlib::memGunzip( $seed->alignment );
  close(S);
  
  $logger->debug("Building hhm");
  
   system($config->hmmer2bin
      . "/hmmbuild "
      .$tempDir."/".$fam->pfama_acc . ".hmm "
      .$tempDir."/".$fam->pfama_acc . ".seed "); 
  
  system($config->hhsearchBin
      . "/hhmake -i "
      .$tempDir."/".$fam->pfama_acc . ".hmm -o "
      .$config->hhsearchLibDir."/".$fam->pfama_acc.".hhm"); 
  
  unlink($tempDir."/".$fam->pfama_acc . ".hmm"); 
  unlink($tempDir."/".$fam->pfama_acc . ".seed"); 
  $logger->debug("Finished hhm");
}

$logger->debug("Getting list of all fams");
#Now cat all the HMMs together, but we do not want to include any obsolete HMMs.
my @allFams = $pfamDB->getSchema->resultset('Pfama')->search;
my %fams;
foreach my $f (@allFams) {
  $fams{ $f->pfama_acc } = $f->auto_pfama;
}


$logger->debug("Making HMM lib");

open( A, ">" . $config->hhsearchLibDir . "/all.hhm" );
opendir( D, $config->hhsearchLibDir );
my @files = readdir(D);
push(@files, "/software/pfam/src/hhsearch/cal.hhm");
foreach my $h (@files) {
  next unless($h =~/PF\d+\.hhm|cal\.hmm/);
  $logger->debug("Adding $h");
  open( H, $config->hhsearchLibDir . "/" . $h );
  while (<H>) {
    print A $_;
  }
}
