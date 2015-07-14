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
my ($status_dir, $pfamseq_dir, $move);
&GetOptions(
  "status_dir=s"  => \$status_dir,
  "pfamseq_dir=s" => \$pfamseq_dir,
  "move"          => \$move);
unless ( $status_dir and -e $status_dir ) {
  help();
}
unless ( $pfamseq_dir and -e $pfamseq_dir ) {
  help();
}
my $cwd = getcwd();


#Create fasta file from uniprot table
#if this is too slow/fails it could be worth trying a smaller offset or sending it to the farm with plenty of memory
my $total=0;
if(-s "$pfamseq_dir/uniprot") {
  $logger->debug("Already made uniprot fasta file");
  my $sth=$dbh->prepare("select count(*) from uniprot") or $logger->logdie("Failed to prepare statement:".$dbh->errstr);
  $sth->execute() or $logger->logdie("Couldn't execute statement ".$sth->errstr);
  $total=$sth->fetchrow();
}
else {
  $logger->debug("Going to make fasta file from uniprot table");
  my $offset = 0;
  my $n = 1;
  open (FA, ">$pfamseq_dir/uniprot") or $logger->logdie("Cannot open $pfamseq_dir/uniprot file to write");
  while (1){
    $logger->debug("Querying uniprot table in database.... chunk $n offset $offset ");
    my $st = $dbh->prepare("select uniprot_acc, seq_version, uniprot_id, description, sequence from uniprot limit 1000000 offset $offset") or $logger->logdie("Failed to prepare statement:".$dbh->errstr);
    $st->execute() or $logger->logdie("Couldn't execute statement ".$st->errstr);
    my $rowno = $st->rows;
    last unless($rowno);
    my $array_ref = $st->fetchall_arrayref();
    $logger->debug("Fetching results...");
    foreach my $row (@$array_ref) {
      print FA ">" . $row->[0] . "." . $row->[1] . " " . $row->[2] . " " . $row->[3] . "\n" . $row->[4] . "\n";
    }
    $offset += 1000000;
    $total += $rowno;
    $n++;
    $logger->debug("$total rows retrieved");
  }
  close FA;
  $dbh->disconnect;
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
if($move) { 
  if (-e "$status_dir/moved_uniprot"){
    $logger->debug("Already moved uniprot to nfs");
  } 
  else {
    $logger->info("Going to delete old uniprot and copy the new one to nfs directory");
    my $uniprot_nfs = $config->{uniprot}->{location};

    my @uniprot_files = qw(uniprot uniprot.ssi);

    foreach my $f (@uniprot_files) {
      $logger->debug("Deleting old $f from $uniprot_nfs");  
      unlink("$uniprot_nfs/$f");
    }

    foreach my $f (@uniprot_files) {
      $logger->debug("Copying new $f to $uniprot_nfs");
      copy("$pfamseq_dir/$f", "$uniprot_nfs/$f") or $logger->logdie("Copy $pfamseq_dir/$f to $uniprot_nfs failed: $!");
    }
    system("touch $status_dir/moved_uniprot") and $logger->logdie("Couldn't touch $status_dir/moved_uniprot:[$!]\n");
  }

  #Change PFAM_CONFIG
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

  print STDERR << "EOF";

This script creates a fasta file from the sequences in the uniprot
table in the database. There is an option to move the uniprot fasta
file to the production location in the Pfam config file, and update 
the config file with the database size. 

Usage:

  $0 -status_dir <status_dir> -pfamseq_dir <pfamseq_dir>

Options
  -help  :Prints this help message
  -move  :Move the pfamseq uniprot file to the production location,
          and update the database size in Pfam config file

Both the status directory and pfamseq_directory must already exist.
pfamseq_dir is the directory where the pfamseq fasta file will be
generated. The status directory is where a log of the progress of 
the script is recorded.

EOF
}
