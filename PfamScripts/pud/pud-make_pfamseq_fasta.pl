#!/usr/bin/env perl

#make the pfamseq fasta file and SSI index
#optionally move pfamseq fasta to production location and update pfam config with database size
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
unless ( $status_dir and -e $status_dir and $rel_num) {
  help();
}
unless ( $pfamseq_dir and -e $pfamseq_dir and $rel_num) {
  help();
}

my $cwd = getcwd();


#Create fasta file from pfamseq table
#if this is too slow/fails it could be worth trying a smaller offset or sending it to the farm with plenty of memory
my $total=0;
if(-s "$pfamseq_dir/pfamseq") {
  $logger->debug("Already made pfamseq fasta file");
  my $sth=$dbh->prepare("select count(*) from pfamseq") or $logger->logdie("Failed to prepare statement:".$dbh->errstr);
  $sth->execute() or $logger->logdie("Couldn't execute statement ".$sth->errstr);
  $total=$sth->fetchrow();
}
else {
  $logger->debug("Going to make fasta file from pfamseq table");
  my $offset = 0;
  my $n = 1;
  open (FA, ">$pfamseq_dir/pfamseq") or $logger->logdie("Cannot open $pfamseq_dir/pfamseq file to write");
  while (1){
    $logger->debug("Querying pfamseq table in database.... chunk $n offset $offset ");
    my $st = $dbh->prepare("select pfamseq_acc, seq_version, pfamseq_id, description, sequence from pfamseq limit 1000000 offset $offset") or $logger->logdie("Failed to prepare statement:".$dbh->errstr);
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
}

#Make DBSIZE file
my $dbsize=$total;
if(-s "$pfamseq_dir/DBSIZE") {
  $logger->debug("Already created $pfamseq_dir/DBSIZE file");
}
else {
  $logger->debug("Creating $pfamseq_dir/DBSIZE file");
  open(DBSIZE, ">$pfamseq_dir/DBSIZE") or die "Couldn't open fh to $pfamseq_dir/DBSIZE, $!";
  print DBSIZE "$dbsize";
  close DBSIZE;
}


#Make easel indexes
if(-e "$status_dir/esl-indexes") { 
  $logger->debug("Already make easel indexes");
} else {
  $logger->debug("Making easel indices for pfamseq");
  chdir($pfamseq_dir) or $logger->logdie("Couldn't chdir into $pfamseq_dir:[$!]");
  system ("esl-sfetch --index pfamseq") and $logger->logdie("Couldn't make easel indices for pfamseq:[$!]");
  chdir($cwd) or $logger->logdie("Couldn't chdir to $cwd:[$!]");
  system("touch $status_dir/esl-indexes") and $logger->logdie("Couldn't touch $status_dir/esl-indexes:[$!]\n");
}

#Copy pfamseq to production location
if (-e "$status_dir/moved_pfamseq"){
  $logger->debug("Already moved pfamseq to nfs");
} 
else {
  my $pfamseq_nfs = $config->{pfamseq}->{location};
  $pfamseq_nfs.=$rel_num;
  $logger->info("Going to copy pfamseq to $pfamseq_nfs");

  unless(-d $pfamseq_nfs) {
    mkdir($pfamseq_nfs, 0775) or $logger->logdie("Couldn't mkdir $pfamseq_nfs, $!");
  }


  foreach my $f ( qw(pfamseq pfamseq.ssi) ) {
    $logger->debug("Copying new $f to $pfamseq_nfs");
    copy("$pfamseq_dir/$f", "$pfamseq_nfs/$f") or $logger->logdie("Copy $pfamseq_dir/$f to $pfamseq_nfs failed: $!");
  }
  system("touch $status_dir/moved_pfamseq") and $logger->logdie("Couldn't touch $status_dir/moved_pfamseq:[$!]\n");
}

if($update_config) {
  if(-e "$status_dir/changed_pfam_config") {
    $logger->info("Already changed database size in Pfam config file\n");
  }
  else {
    $logger->info("Updating database size in Pfam config file\n");

    my $c = new Config::General($ENV{PFAM_CONFIG}); 
    my %ac = $c->getall; 

    my $pfam_config = $ENV{PFAM_CONFIG};
    move($pfam_config, "$pfam_config.old") or $logger->logdie("Could not move config file");;
    $ac{pfamseq}->{dbsize} = $dbsize; 

    SaveConfig($pfam_config, \%ac);

    my $newConfig;
    eval { 
      $newConfig = Bio::Pfam::Config->new; 

    };  
    if($@) { 
      $logger->logdie("Problem modifying the pfam_config ($pfam_config) file");
    }
    $config = $newConfig;

    system("touch $status_dir/changed_pfam_config") and $logger->logdie("Couldn't touch $status_dir/changed_pfam_config:[$!]\n"); 
  }
}



sub help{
  my $loc=$config->{pfamseq}->{location};
  print STDERR << "EOF";

This script creates a fasta file from the sequences in the pfamseq
table in the database. It also copies the pfamseq fasta
file to the production location in the Pfam config file. Optionally
it can update the config file with the database size. 

Usage:

  $0 -status_dir <status_dir> -pfamseq_dir <pfamseq_dir> -rel 29

Options
  -help           :Prints this help message
  -update_config  :Update the database size in Pfam config file

Both the status directory and pfamseq_directory must already exist.
pfamseq_dir is the directory where the pfamseq fasta file will be
generated. The status directory is where a log of the progress of 
the script is recorded. The -rel flag is mandatory. A copy of the 
pfamseq fasta file will be copied to $loc<rel_num>

EOF
 exit;
}
