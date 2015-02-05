#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;
use LWP::UserAgent;
use Getopt::Long;
use Log::Log4perl qw(:easy);
use Archive::Tar;
use File::Touch;
use File::Copy;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my ( $statusdir, $pfamseq_dir );

#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

&GetOptions(
  "status_dir=s"  => \$statusdir,
  "pfamseq_dir=s" => \$pfamseq_dir
  )
  or $logger->logdie("Invalid option!\n");

unless ( $statusdir and -e $statusdir ) {
  help();
}
unless ( $pfamseq_dir and -e $pfamseq_dir ) {
  help();
}

my $pwd = cwd;
chdir($pfamseq_dir)
  or $logger->logdie("Couldn't change directory into $pfamseq_dir $!\n");

#Database connection stuff
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

#Copy over AntiFam hmms and relnotes
my $antifamdir = $config->antifamLoc;
copy ("$antifamdir/AntiFam.hmm","AntiFam.hmm") or $logger->logdie("Could not copy AntiFam.hmm [$!]\n");
copy ("$antifamdir/relnotes","relnotes") or $logger->logdie("Could not copy relnotes [$!]\n");

#Now run pfamseq against antifam.....
if ( -e 'matches' ) {
  $logger->info("Looks like antifam search has already been run!");
}
else {
    $logger->info( "Running antifam against pfamseq. ");
#run AntiFam on the farm
    my $fh         = IO::File->new();

    $fh->open( "| bsub -q production-rh6 -R \"select[mem>2000] rusage[mem=2000]\" -M 2000000 -o runantifam.log -Jantifam") or $logger->logdie("Couldn't open file handle [$!]\n");
    $fh->print( "hmmsearch --cpu 8 --noali --cut_ga --tblout matches AntiFam.hmm pfamseq\n"); 
    $fh->close; 
#have jobs finished?
    if (-e "$statusdir/finishedantifam"){
	$logger->info("Already checked AntiFam search has finished\n");
    } else {
	my $fin = 0;
	while (!$fin){
	    open( FH, "bjobs -Jantifam|" );
	    my $jobs;
	    while (<FH>){
		if (/^\d+/){
		    $jobs++;
		}
	    }
	    close FH;
	    if ($jobs){
		$logger->info("AntiFam search still running - checking again in 10 minutes\n");
		sleep(600);
	    } else {
		$fin = 1;
		open( FH, "> $statusdir/finishedantifam" ) or die "Can not write to file $statusdir/finishedantifam.txt";
		close(FH);
	    }
	}
    }

}

open( M, "<", "matches" ) or $logger->logdie("Failed to open matches");
my %seqsToDel;
my %antifamMap;

#Should get somthing like this.
##                                                               --- full sequence ---- --- best 1 domain ---- --- domain number estimation ----
## target name        accession  query name           accession    E-value  score  bias   E-value  score  bias   exp reg clu  ov env dom rep inc description of target
##------------------- ---------- -------------------- ---------- --------- ------ ----- --------- ------ -----   --- --- --- --- --- --- --- --- ---------------------
#Q72TI8.1             -          Spurious_ORF_01      ANF00001     1.3e-25   98.9   7.8   1.5e-25   98.7   5.4   1.1   1   0   0   1   1   1   1 Q72TI8_LEPIC Putative uncharacterized protein
#G7QEZ5.1             -          Spurious_ORF_01      ANF00001     2.8e-22   88.2   6.2   4.2e-22   87.6   4.3   1.2   1   0   0   1   1   1   1 G7QEZ5_LEPII Putative uncharacterized protein
#Q8F2S5.1             -          Spurious_ORF_01      ANF00001     2.8e-22   88.2   6.2   4.2e-22   87.6   4.3   1.2   1   0   0   1   1   1   1 Q8F2S5_LEPIN Putative uncharacterized protein
#Q72SR8.1             -          Spurious_ORF_01      ANF00001     6.6e-21   83.8   5.2     1e-20   83.2   3.6   1.2   1   0   0   1   1   1   1 Q72SR8_LEPIC Putative uncharacterized protein
#Q7UHL1.1             -          Spurious_ORF_02      ANF00002     1.9e-50  179.7   2.1   2.5e-50  179.3   1.5   1.1   1   0   0   1   1   1   1 Q7UHL1_RHOBA Putative uncharacterized protein
#Q7UN59.1             -          Spurious_ORF_02      ANF00002       2e-47  169.9   0.5     2e-47  169.9   0.4   1.5   2   0   0   2   2   1   1 Q7UN59_RHOBA Putative uncharacterized protein
#Q7UWA0.1             -          Spurious_ORF_02      ANF00002     5.6e-46  165.3   5.6   5.6e-46  165.3   3.9   1.9   2   0   0   2   2   1   1 Q7UWA0_RHOBA Putative uncharacterized protein
#Q7ULC0.1             -          Spurious_ORF_02      ANF00002     6.4e-45  161.9   2.2   2.4e-44  160.1   0.5   2.2   2   0   0   2   2   1   1 Q7ULC0_RHOBA Putative uncharacterized protein
#A9LGZ9.1             -          Spurious_ORF_02      ANF00002     5.3e-15   65.5   0.1   6.2e-15   65.3   0.0   1.1   1   0   0   1   1   1   1 A9LGZ9_9BACT Putative uncharacterized protein

#Check that we have at least one domain included!
while (<M>) {
  next if (/^#/);
  my @row = split( /\s+/, $_ );
  next if ( $row[17] == 0 );
  $seqsToDel{ $row[0] }         = 0;
  $antifamMap{ $row[0] }->{acc} = $row[3];
  $antifamMap{ $row[0] }->{id}  = $row[3];
}

if ( !-e "$statusdir/updated_pfamseq_antifam" ) {

  #Delete sequecnes from the databases. 
  my $sthPfamseq = $dbh->prepare(
    "SELECT pfamseq_id, 
                                       pfamseq_acc, 
                                       seq_version, 
                                       crc64, 
                                       md5, 
                                       description, 
                                       evidence, 
                                       length, 
                                       species, 
                                       taxonomy, 
                                       is_fragment, 
                                       sequence, 
                                       ncbi_taxid 
                                       FROM pfamseq WHERE pfamseq_acc=?"
  );

  my $sthPfamseqAntifam = $dbh->prepare(
    "INSERT INTO pfamseq_antifam (pfamseq_id, 
                                                            pfamseq_acc, 
                                                            seq_version, 
                                                            crc64, 
                                                            md5, 
                                                            description, 
                                                            evidence, 
                                                            length, 
                                                            species, 
                                                            taxonomy, 
                                                            is_fragment, 
                                                            sequence, 
                                                            ncbi_taxid, 
                                                            antifam_acc, 
                                                            antifam_id ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
  );

  my $sthDel = $dbh->prepare("DELETE FROM pfamseq WHERE pfamseq_acc=?");

  $dbh->begin_work;
  my $c = 0;
  while ( my ( $key, $value ) = each %seqsToDel ) {
      #Get the data we need into pfamseq_antifam
      my ( $acc, $version ) = split( /\./, $key );
      $sthPfamseq->execute($acc) or $logger->logdie( $dbh->errstr );
      my $data = $sthPfamseq->fetchrow_arrayref;
      unless ( defined($data) and ref($data) eq 'ARRAY' ) {
        $logger->logdie("failed to find sequence data for $acc");
      }
      my $newData;
      foreach (@$data){
        push(@$newData, $_);  
      }
      #Now push on the antifam data
      push( @$newData, $antifamMap{$key}->{acc}, $antifamMap{$key}->{id} );

      #Add the data to the antifam table
      $sthPfamseqAntifam->execute(@$newData) or $logger->logdie( $dbh->errstr );

      #Now detele the row from pfamseq.
      $sthDel->execute($acc) or $logger->logdie( $dbh->errstr );

      $c++;
      if ( $c == 500 ) {
        $dbh->commit or $logger->logdie( $dbh->errstr );
        $dbh->begin_work;
        $c = 0;
      }
  }
  $dbh->commit or $logger->logdie( $dbh->errstr );
  touch("$statusdir/updated_pfamseq_antifam");
}
else {
  $logger->info("Already updated pfamseq_antifam\n");
}

chdir($pwd);
