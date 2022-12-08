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


#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();


my ($status_dir, $pfamseq_dir, $logs_dir, $help);


GetOptions(
  'help'          => \$help,
  'status_dir=s'  => \$status_dir,
  'pfamseq_dir=s' => \$pfamseq_dir,
  'logs_dir=s'    => \$logs_dir
) or $logger->logdie("Invalid option!\n");


if ($help) {
  help();
}

unless ( $status_dir and -e $status_dir ) {
  print "status_dir is required\n\n";
  help();
}

unless ( $pfamseq_dir and -e $pfamseq_dir ) {
  print "pfamseq_dir is required\n\n";
  help();
}



$logs_dir //= "${status_dir}";

unless ( $logs_dir and -e $logs_dir ) {
  print "logs_dir does not exist\n\n";
  help();
}

$logs_dir .= "/Antifam_logs";
my $hmms_dir = "${pfamseq_dir}/Antifam_hmms";
my $matches_dir = "${pfamseq_dir}/Antifam_matches";






#Database connection stuff
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;


#Copy over AntiFam hmms and relnotes
my $antifam_dir = $config->antifamLoc;

my $uniprot_seq_file = $pfamseq_dir . '/uniprot.fasta';

if (! -e $uniprot_seq_file) {
  $logger->logdie("Can't find seq file '$uniprot_seq_file': $!\n");
}





if ( !-e "${status_dir}/antifam_hmms_run" ) {

  mkdir($logs_dir)
    or $logger->logdie("Can't create $logs_dir $!\n");

  mkdir($matches_dir)
    or $logger->logdie("Can't create $matches_dir $!\n");

  mkdir($hmms_dir)
    or $logger->logdie("Can't create $hmms_dir $!\n");

  $logger->info("Getting AntiFam data...\n");
  my $antifam_list = &copy_antifam_data($antifam_dir, $hmms_dir);


  my (@completed, @error);

  while (scalar @completed + scalar @error != scalar @{$antifam_list}) {

    my @running_jobs = split( "\n", `bjobs -w | grep '_rAFM'| awk '{print \$7}'` );

    if (@running_jobs) {
      $logger->info("AntiFam search still running - checking again in 30 minutes\n");
      sleep(1800);
    }

    foreach my $antifam_id (@{$antifam_list}) {
      my $matches_file = $matches_dir . '/' . $antifam_id . '_matches';
      my $log_file = $logs_dir . '/' . $antifam_id . '.log';
      my $hmm_file = "${hmms_dir}/${antifam_id}.hmm";

      if (! -e $log_file) {
        if ( ! grep /$antifam_id/, @running_jobs ) {
          my $queue = $config->{farm}->{lsf}->{queue};
          system("bsub -q $queue -R \"select[mem>2000] rusage[mem=2000]\" -n 8 -M 8000 -o $log_file -g /Pfam_100 -J ${antifam_id}_rAFM hmmsearch --cpu 8 --noali --cut_ga --tblout $matches_file $hmm_file $uniprot_seq_file")
            and die "Error submitting hmmsearch job:[$!]";
        }
      } else {
        if ( (! grep /$antifam_id/, @error) && (! grep /$antifam_id/, @completed) ) {
          if (-s $log_file) {
            open my $log_fh, '<', $log_file or die;
            my $log = do { local $/; <$log_fh> };
            if ( $log =~ m/Successfully completed/) {
              $logger->info("$antifam_id search has completed.\n");
              push @completed, $antifam_id;
            } else {
              $logger->info("Error processing $antifam_id search.\n");
              push @error, $antifam_id;
            }
          } else {
            $logger->info("$antifam_id search is in progress.\n");
          }
        }
      }
    }

  }
  $logger->info("All AntiFam matches searches have completed.\n");


  if (scalar @error) {
    $logger->logdie("The following searches failed to run: @error\n");
  } else {
      touch("${status_dir}/antifam_hmms_run");
  }

} else {
  $logger->info("Antifam HMMs have been completed previously. Skipping...\n");
}





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


my %seqsToDel;
my %antifamMap;

foreach my $matches_file ( glob("${matches_dir}/*_matches") ) {

  open(my $match_fh, '<', "$matches_file") or $logger->logdie("Failed to open matches");

  #Check that we have at least one domain included!
  while (my $line = <$match_fh>) {
    next if ($line =~ /^#/);

    my @row = split( /\s+/, $line );

    next if ( $row[17] == 0 );
    $seqsToDel{ $row[0] }         = 0;
    $antifamMap{ $row[0] }->{acc} = $row[3];
    $antifamMap{ $row[0] }->{id}  = $row[2];
  }

}


my $antifamSeq = keys %seqsToDel // 0;


$logger->info("$antifamSeq sequences in uniprot match antifam, going to delete them from the uniprot and pfamseq tables and add them to the pfamseq_antifam table");
if ( !-e "${status_dir}/updated_pfamseq_antifam" ) {

  $logger->info("Going to delete sequences currently in the pfamseq_antifam table");
  my $stDelAntiSeq=$dbh->prepare("delete from pfamseq_antifam");
  $stDelAntiSeq->execute() or $logger->logdie( $dbh->errstr );


  #Delete sequences from the databases.
  my $sthUniProt = $dbh->prepare(
    "SELECT uniprot_id, 
    uniprot_acc, 
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
    FROM uniprot WHERE uniprot_acc=?"
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

  my $sthDelPfamseq = $dbh->prepare("DELETE FROM pfamseq WHERE pfamseq_acc=?");
  my $sthDelUniProt = $dbh->prepare("DELETE FROM uniprot WHERE uniprot_acc=?");

  $dbh->begin_work;
  my $c = 0;
  while ( my ( $key, $value ) = each %seqsToDel ) {
    #Get the data we need into pfamseq_antifam
    my ( $acc, $version ) = split( /\./, $key );
    $sthUniProt->execute($acc) or $logger->logdie( $dbh->errstr );
    my $data = $sthUniProt->fetchrow_arrayref;
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

    #Now delete the row from pfamseq and uniprot
    $sthDelPfamseq->execute($acc) or $logger->logdie( $dbh->errstr );
    $sthDelUniProt->execute($acc) or $logger->logdie( $dbh->errstr );

    $c++;
    if ( $c == 500 ) {
      $dbh->commit or $logger->logdie( $dbh->errstr );
      $dbh->begin_work;
      $c = 0;
    }
  }
  $dbh->commit or $logger->logdie( $dbh->errstr );
  # chdir($pwd) or $logger->logdie("Couldn't chdir to $pwd [$!]");
  touch("${status_dir}/updated_pfamseq_antifam");
}
else {
  $logger->info("Already updated pfamseq_antifam\n");
}

#Check uniprot and antifam tables have the correct number of entries
my $sthU = $dbh->prepare("select count(*) from uniprot");
$sthU->execute();
my $uniprotRdb=0;
$uniprotRdb=$sthU->fetchrow();
my $sthA= $dbh->prepare("select count(*) from pfamseq_antifam");
$sthA->execute();
my $antifamRdb=0;
$antifamRdb=$sthA->fetchrow();
my $uniprotPreAntifam;
open(FH, "$pfamseq_dir/DBSIZE_uniprot_preAntifam") or $logger->logdie("Couldn't open fh to DBSIZE_uniprot_preAntifam [$!]");
while(<FH>) {
  if(/(\d+)/) {
    $uniprotPreAntifam=$1;
  }
}
close FH;

unless($uniprotPreAntifam) {
  $logger->logdie("Couldn't obtain db size from $pfamseq_dir/DBSIZE_uniprot_preAntifam");
}

my $expUniprotSeq=$uniprotPreAntifam-$antifamSeq;
if($expUniprotSeq==$uniprotRdb) {
  $logger->info("The uniprot table has had $antifamSeq sequences removed, and now contains $uniprotRdb sequences");
}
else {
  $logger->logdie("The Uniprot table does not have the expected number of sequences (should be $uniprotPreAntifam-$antifamSeq=$expUniprotSeq");
}
if($antifamSeq==$antifamRdb) {
  $logger->info("The pfamseq_antifam table contains $antifamSeq sequences");
}
else {
  $logger->logdie("The pfamseq_antfam table contins $antifamRdb sequences, but should contain $antifamSeq sequences");
}

#Print the number of pfamseq sequences that have been removed
my $pfamseqPreAntifam;
open(FH, "$pfamseq_dir/DBSIZE_pfamseq_preAntifam") or $logger->logdie("Couldn't open fh to DBSIZE_pfamseq_preAntifam [$!]");
while(<FH>) {
  if(/(\d+)/) {
    $pfamseqPreAntifam=$1;
  }
}
close FH;

my $sthP = $dbh->prepare("select count(*) from pfamseq");
$sthP->execute();
my $pfamseqRdb=0;
$pfamseqRdb=$sthP->fetchrow();
my $pfamseqDeleted=$pfamseqPreAntifam-$pfamseqRdb;
$logger->info("The pfamseq table has had $pfamseqDeleted sequences removed, and now contains $pfamseqRdb sequences");





sub copy_antifam_data {
  my ($antifam_dir, $hmms_dir) = @_;

  my $hmm_file = $antifam_dir . '/AntiFam.hmm';

  open(my $hmm_in, '<', "$hmm_file") or die "Can't open '$hmm_file': $!\n";


  my (@curr_hmm, $hmm_name, @hmm_list);
  while (my $line = <$hmm_in>) {
    # save current hmm
    push @curr_hmm, $line;

    # get acc of hmm to name file
    if ($line =~ m/ACC\s+(.*)?\n/ ) {
      $hmm_name = $1;
      push @hmm_list, $hmm_name;
    }

    # end of hmm, print it out and clear up for next one
    if ($line =~ m/\/\// ) {
      open(my $hmm_out, '>', "${hmms_dir}/${hmm_name}.hmm") or die "Can't open '${hmm_name}.hmm': $!\n";
      print $hmm_out join($", @curr_hmm);
      close($hmm_out);

      @curr_hmm = ();
    }

  }

  # copy ("$antifam_dir/relnotes","relnotes") or $logger->logdie("Could not copy relnotes [$!]\n");


  return \@hmm_list;

}


sub help{
  print STDERR << "EOF";

This script creates a fasta file from the sequences in the uniprot
table in the database. It also copies the uniprot fasta
file to the production location in the Pfam config file. Optionally
it can update the config file with the database size. 

Usage:

  $0 -status_dir <status_dir> -pfamseq_dir <pfamseq_dir> [-logs_dir <logs_dir>]

Options
  -help           :Prints this help message

AntiFam is a collection of HMMs designed to identify spurious protein predictions.
The pud-removeAntiFamMatches.pl script retrieves the current AntiFam hmm file, 
uses hmmsearch to run the AntiFam models against the uniprot.fasta file created above, 
and removes any matches from the pfamseq and uniprot tables in the pfam_release database. 
It also uploads the sequences that match antifam to the pfamseq_antifam table in the pfam_release database.

Both the status directory and pfamseq_directory must already exist.
pfamseq_dir is the directory where the uniprot and pfamseq data exist and the antifam matches are stored before insertion into the database.
The status directory is where the progress of the script is recorded.
The logs_dir is optional and status_dir will be used if none provided, this is where logs are stored.

EOF
  exit;
}
