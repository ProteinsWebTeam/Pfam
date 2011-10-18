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


my $split = shift;

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

#-------------------------------------------------------------------------------
# Get connection to the jobs database, find the job, and find when this type of
# job was last successfully run!
#Page the results.....

my $pfamAs =
  $pfamDB->getSchema->resultset('Pfama')->search(
  {},
  {
    page => 1,
	  rows => 100 
  } 
  );

my $pager = $pfamAs->pager;

#for(my $n = 1;  $n <= $pager->last_page; $n++){
  $logger->debug("Working on page $split out of ".$pager->last_page);
  my @pfamAs = $pfamAs->page($split)->all; 


my @pfamAaccs = $pfamDB->getSchema->resultset('Pfama')->search({});
my %fams;
foreach my $p (@pfamAaccs){
  $fams{$p->pfama_acc} =$p->auto_pfama;
}

foreach my $fam (@pfamAs) {
  $logger->debug("Now searching ".$fam->pfama_acc );
  #First pass to calibrate the hmm
  
  #/software/pfam/src/hhsearch/hhsearch -cal -i PF00001.hmm -d all.hmm
  system( $config->hhsearchBin
      . "/hhsearch -cal -i "
      . $config->hhsearchLibDir . "/"
      . $fam->pfama_acc
      . ".hhm -d "
      . $config->hhsearchLibDir
      . "/all.hhm" );

  #Second pass to accurately score the hmm.
  #hsearch -i PF00001.hmm -d all.hmm
  open( R,
        $config->hhsearchBin
      . "/hhsearch -i "
      . $config->hhsearchLibDir . "/"
      . $fam->pfama_acc
      . ".hhm -d "
      . $config->hhsearchLibDir
      . "/all.hhm |" );

  my $qAcc;
  while (<R>) {
    if (
/\d+\s+(PF\d{5}).{23}\s+\S+\s+(\S+)\s\S+\s+\S+\s+\S+\s+\S+\s+(\d+)\-(\d+)\s+(\d+)\-(\d+)/
      )
    {
      next unless ( $2 <= 0.01 );
      my $tAcc  = $1;
      my $score = $2;
      my $qFrom = $3;
      my $qTo   = $4;
      my $tFrom = $5;
      my $tTo   = $6;

      if ( $fams{$tAcc} and $fams{$qAcc} ) {

        #Will probably die and require a unique contraint
        $pfamDB->getSchema->resultset('Pfama2pfamaHhsearchResults')
          ->update_or_create(
          {
            auto_pfama1 => $fams{$qAcc},
            auto_pfama2 => $fams{$tAcc},
            evalue      => $score
          },
          { key => 'pairwise' }
          );

        #We and things to be symetrical
        $pfamDB->getSchema->resultset('Pfama2pfamaHhsearchResults')
          ->update_or_create(
          {
            auto_pfama2 => $fams{$qAcc},
            auto_pfama1 => $fams{$tAcc},
            evalue      => $score
          },
          { key => 'pairwise' }
          );

      }

    }
    elsif (/Query\s+(PF\d{5})/) {
      $qAcc = $1;
    }
    elsif (/^No 1/) {

      #We are on to the alignment section, so skip this!
      last;
    }
  }
  close(R);
}

