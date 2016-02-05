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


#Create fasta file from uniprot table
my $total=0;
if(-s "$pfamseq_dir/uniprot") {
  $logger->debug("Already made uniprot fasta file");
  my $sth=$dbh->prepare("select count(*) from uniprot") or $logger->logdie("Failed to prepare statement:".$dbh->errstr);
  $sth->execute() or $logger->logdie("Couldn't execute statement ".$sth->errstr);
  $total=$sth->fetchrow();
}
else {
  $logger->debug("Going to make fasta file from uniprot table");
  my $uniprot_file="uniprot.$$";
  my $command = "mysql -h ".$pfamDB->{host}." -u ".$pfamDB->{user}." -p". $pfamDB->{password}." -P ".$pfamDB->{port}." ".$pfamDB->{database}." --quick -e 'select uniprot_acc, seq_version, uniprot_id, description, sequence from uniprot' > $uniprot_file";
  system("$command") and $logger->logdie ("Couldn't run mysql command [$command], $!");
  open(UNIPROT, $uniprot_file) or $logger->logdie ("Couldn't open $uniprot_file, $!");
  open (FA, ">$pfamseq_dir/uniprot") or $logger->logdie("Cannot open $pfamseq_dir/uniprot file to write");
  while(<UNIPROT>) {
    next if(/^uniprot_acc/); #Skip header
    chomp;
    my ($uniprot_acc, $seq_version, $uniprot_id, $desc, $sequence) = split(/\t/, $_);
    print FA ">$uniprot_acc.$seq_version $uniprot_id $desc\n$sequence\n";
    $total++;
  }
  close FA;
  close UNIPROT;
  unlink($uniprot_file);
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

my $dbsize=$total;

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

This script creates a fasta file from the sequences in the uniprot
table in the database. It also copies the uniprot fasta
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
