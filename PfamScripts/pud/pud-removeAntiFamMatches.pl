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

my $antiFamURL =
  "ftp://ftp.sanger.ac.uk/pub/databases/Pfam/AntiFam/current/AntiFam.tar.gz";

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

#Fetch the antifam file using LWP, we may have in internally...
my $ua          = LWP::UserAgent->new;
my $antiFamFile = 'AntiFam.tar.gz';
my $response    = $ua->mirror( $antiFamURL, $antiFamFile );
if ( $response->is_success ) {
  $logger->info("Successfully download $antiFamFile.");
}
elsif ( $response->code == HTTP::Status::RC_NOT_MODIFIED ) {
  $logger->info("File $antiFamFile: up-to-date.");
}
else {
  $logger->logdie( 'Failed, got '
      . $response->status_line . ' for '
      . $response->request->uri
      . "." );
}

my $output = 'AntiFam.tar';

#gunzip $antiFamFile => $output or die "gunzip failed: $GunzipError\n";
my $tar = Archive::Tar->new;
$tar->read($antiFamFile);

#Now extract the release information
$tar->extract('relnotes') if ( !-e 'relnotes' );

#Exract the HMM out of the file.
$tar->extract('AntiFam.hmm') if ( !-e 'AntiFam.hmm' );

#Now run pfamseq against antifam.....
if ( -e 'matches' ) {
  $logger->info("Looks like antifam search has already been run!");
}
else {
  $logger->info( "Running antifam against pfamseq. "
      . "In the future, this may need to be run on the farm." );

  system("hmmsearch --cpu 8 --cut_ga --tblout matches AntiFam.hmm pfamseq")
    and $logger->logdie("Failed to run hmmsearch with antifam/pfamseq.");
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
my $num_seq = 0;
unless ( -e "$statusdir/made_pfamseq_antifam" ) {

  #Remove sequences that are matched, re-write pfamseq.
  open( P, "<", "pfamseq" ) or $logger->logdie("Could not open pfamseq:[$!]\n");
  open( S, ">", "pfamseq.antifam" )
    or $logger->logdie("Could not open pfamseq.antifam:[$!]\n");

  my $ok = 1;

  while (<P>) {
    if (/^>(\S+)/) {
      $ok = exists( $seqsToDel{$1} ) ? 0 : 1;
      if ($ok) {
        $num_seq++;
      }
      else {
        $logger->info("Excluding $1");
        $seqsToDel{$1} = 1;
      }
    }
    print S $_ if ($ok);
  }
  close(P);
  close(S);

  #Make DBSIZE file

  $logger->debug("Making DBSIZE file\n");
  open( DBSIZE, ">DBSIZE" )
    or $logger->logdie("Couldn't open filehandle to DBSIZE:[$!]\n");
  print DBSIZE "$num_seq";
  close DBSIZE;
  touch("$statusdir/made_pfamseq_antifam");
}
else {
  open( DBSIZE, "<", "DBSIZE" )
    or $logger->logdie("Could not open DBSIZE for reading:[$!]");
  while (<DBSIZE>) {
    chomp;
    $num_seq = $_;
    last;
  }
  close(DBSIZE);

  $logger->info("Already made pfamseq antifam\n");
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

if(-e "$statusdir/verified_and_moved_pfamseq"){
  $logger->info("Already verified modified pfamseq");
}else{
  my $pfamseq_qc = $dbh->prepare("select count(*) from pfamseq");

$pfamseq_qc->execute
  or $logger->logdie(
  "Failed to query pfamseq for size of pfamseq " . $pfamseq_qc->errstr . "\n" );

my $rdb_size = $pfamseq_qc->fetchrow;

if ( $rdb_size != $num_seq ) {
  $logger->logdie(
"Mis-match between [$rdb_size] sequences in rdb, and $num_seq sequences in the antifam pfamseq fasta file\n"
  );
}
else {
  #Move pfamseq sideways
  move("pfamseq", "pfamseq.preantifam")
    or $logger->logdie("Failed to move pfamseq to pfamseq.preantifam");
 
  #Move pfamseq.antifam to pfamseq
   move("pfamseq.antifam", "pfamseq")
    or $logger->logdie("Failed to move pfamseq.antifam to pfamseq");
  #unlink the ssi
  unlink("pfamseq.ssi") or $logger->logdie("Failed to unlink pfamseq.ssi");
  }
  touch("$statusdir/verified_and_moved_pfamseq")
    and $logger->logdie("Could not touch $statusdir/check_pfamseq_size:[$!]");  
  $logger->info("Verified modified pfamseq");
}

#Make NCBI and WU-blast indices
if ( -e "$statusdir/made_easel_indices2" ) {
  $logger->debug("Already made easel indices for pfamseq\n");
}
else {
  $logger->debug("Making easel indices for pfamseq\n");
  system("esl-sfetch --index pfamseq")
    and $logger->logdie("Couldn't make easel indices for pfamseq:[$!]");
  system("touch $statusdir/made_easel_indices2")
    and $logger->logdie("Couldn't touch $statusdir/made_easel_indices:[$!]\n");
}

if ( -e "$statusdir/made_ncbi_indices2" ) {
  $logger->debug("Already made NCBI indices for pfamseq\n");
}
else {
  $logger->debug("Making NCBI indices for pfamseq\n");
  system("formatdb -p T -i pfamseq")
    and $logger->logdie("Couldn't make NCBI-blast indices for pfamseq:[$!]");
  system("touch $statusdir/made_ncbi_indices2")
    and $logger->logdie("Couldn't touch $statusdir/made_ncbi_indices:[$!]\n");
}
chdir($pwd);