#!/usr/bin/env perl

#make the uniprot fasta file and SSI index
use strict;
use warnings;
use Getopt::Long;
use Log::Log4perl qw(:easy);
use File::Copy;
use Cwd;
use Config::General qw(SaveConfig);
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

#Database connection stuff
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;



#User options
my ($status_dir, $pfamseq_dir, $rel_num, $move, $update_config, $help);
&GetOptions(
  "help"          => \$help,
  "status_dir=s"  => \$status_dir,
  "pfamseq_dir=s" => \$pfamseq_dir,
  "rel=i"         => \$rel_num,
  "update_config" => \$update_config);

if($help) {
  help();
}
unless($rel_num) {
  help();
}
unless ( $status_dir and -e $status_dir ) {
  help();
}
unless ( $pfamseq_dir and -e $pfamseq_dir ) {
  help();
}
my $cwd = getcwd();


#Create fasta file
my $total = 0;
if(-s "${pfamseq_dir}/uniprot") {
  $logger->debug("Already made uniprot fasta file");
  $total = -1;
}
else {
  if(!-s "${pfamseq_dir}/uniprot.fasta") {
    $logger->logdie("Source ${pfamseq_dir}/uniprot.fasta does not seem to exist. Abort.");
  }
  $logger->debug("Getting Antifam sequence accessions from database.");

  my $sthAntifam = $dbh->prepare(
    "SELECT pfamseq_acc, seq_version FROM pfamseq_antifam"
  );

  $sthAntifam->execute() or $logger->logdie( $dbh->errstr );

  my %antifam;
  while (my ($pfamseq_acc, $seq_version) = $sthAntifam->fetchrow_array()) {
    $antifam{"${pfamseq_acc}.${seq_version}"} = 1;
  }

  $sthAntifam->finish();

  $logger->debug("Going to create fasta file without antifam sequences (${pfamseq_dir}/uniprot) based on the ${pfamseq_dir}/uniprot.fasta file.");

  open my $source_fh, '<', "${pfamseq_dir}/uniprot.fasta" or $logger->logdie ("Couldn't open source uniprot.fasta file, $!");
  open my $target_fh, '>', "${pfamseq_dir}/uniprot" or $logger->logdie ("Couldn't open target uniprot file, $!");

  my $write = 0;
  while (my $line = <$source_fh>) {
    if ( $line =~ m/>(\w+\.\d+)/) {
      my $uniprot_id = $1;
      if ($antifam{$uniprot_id}) {
        $write = 0;
      } else {
        $write = 1;
        $total++;
      }
    }

    if ($write) {
      print $target_fh $line;
    }
  }
  close $source_fh;
  close $target_fh;
}

$logger->debug("Getting uniprot sequence count from database.");
my $sth = $dbh->prepare("select count(*) from uniprot") or $logger->logdie("Failed to prepare statement:".$dbh->errstr);
$sth->execute() or $logger->logdie("Couldn't execute statement ".$sth->errstr);
my $dbsize = $sth->fetchrow();
$sth->finish();

$dbh->disconnect();

# fasta file existed, will trust db counts
if ($total == -1) {
  $total = $dbsize;
}

if ($dbsize != $total) {
  $logger->logdie("Wrote ${total} sequences into ${pfamseq_dir}/uniprot file but database contains ${dbsize}. Abort.\n");
}

#Make easel indexes
if(-e "$status_dir/esl-indexes_uniprot") { 
  $logger->debug("Already make easel indexes");
} else {
  $logger->debug("Making easel indices for uniprot");
  chdir($pfamseq_dir) or $logger->logdie("Couldn't chdir into $pfamseq_dir:[$!]");
  system ("esl-sfetch --index uniprot") and $logger->logdie("Couldn't make easel indices for uniprot:[$!]");
  chdir($cwd) or $logger->logdie("Couldn't chdir to $cwd:[$!]");
  system("touch $status_dir/esl-indexes_uniprot") and $logger->logdie("Couldn't touch $status_dir/esl-indexes_uniprot:[$!]\n");
}

#Copy uniprot to production location
if (-e "$status_dir/moved_uniprot"){
  $logger->debug("Already moved uniprot to nfs");
} 
else {
  my $uniprot_nfs = $config->{uniprot}->{location};
  $uniprot_nfs.=$rel_num;

  $logger->info("Going to copy uniprot to $uniprot_nfs");

  unless(-d $uniprot_nfs) {
    mkdir($uniprot_nfs, 0775) or $logger->logdie("Couldn't mkdir $uniprot_nfs, $!");
  }

  foreach my $f ( qw(uniprot uniprot.ssi) ) {
    $logger->debug("Copying new $f to $uniprot_nfs");
    copy("$pfamseq_dir/$f", "$uniprot_nfs/$f") or $logger->logdie("Copy $pfamseq_dir/$f to $uniprot_nfs failed: $!");
  }
  system("touch $status_dir/moved_uniprot") and $logger->logdie("Couldn't touch $status_dir/moved_uniprot:[$!]\n");
}

if($update_config) {
  if(-e "$status_dir/changed_uniprot_config") {
    $logger->info("Already changed database size in Pfam config file\n");
  }
  else {
    $logger->info("Updating database size in Pfam config file\n");

    my $c = new Config::General($ENV{PFAM_CONFIG}); 
    my %ac = $c->getall; 

    my $pfam_config = $ENV{PFAM_CONFIG};
    move($pfam_config, "$pfam_config.old") or $logger->logdie("Could not move config file");;
    $ac{uniprot}->{dbsize} = $dbsize; 

    SaveConfig($pfam_config, \%ac);

    my $newConfig;
    eval { 
      $newConfig = Bio::Pfam::Config->new; 

    };  
    if($@) { 
      $logger->logdie("Problem modifying the pfam_config ($pfam_config) file");
    }
    $config = $newConfig;

    system("touch $status_dir/changed_uniprot_config") and $logger->logdie("Couldn't touch $status_dir/changed_uniprot_config:[$!]\n"); 
  }
}


sub help{

  my $loc=$config->{uniprot}->{location};
  print STDERR << "EOF";

This script creates a uniprot fasta file based on the uniprot.fasta
fasta file, excluding AntiFam sequences.
Those sequences should match the sequences in the uniprot
table in the database.  It also copies the uniprot fasta
file to the production location in the Pfam config file. Optionally
it can update the config file with the database size. 

Usage:

  $0 -status_dir <status_dir> -pfamseq_dir <pfamseq_dir> -rel 29

Options
  -help           :Prints this help message
  -update_config  :Update the database size in Pfam config file

Both the status directory and pfamseq_directory must already exist.
pfamseq_dir is the directory where the uniprot fasta file will be
generated. The status directory is where a log of the progress of 
the script is recorded. The -rel flag is mandatory. A copy of the 
pfamseq fasta file will be copied to $loc<rel_num>.

EOF
  exit;
}
