#!/usr/bin/env perl

#Script to discard non RP seq from SEED
#Input is SEED alignment
#Output is a file called 'SEED.rp' that contains only the seq in SEED that are in RP

use strict;
use warnings;
use DBI;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;


my $help;
GetOptions('help' => \$help);

if($help) {
  help();
}


my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;

my $aln="SEED";
unless(-s $aln) {
 die "$aln does not exist";
}

my $sth=$dbh->prepare("select pfamseq_acc from pfamseq where pfamseq_acc=? and seq_version=?");
open(OUT, ">SEED.rp") or die "Couldn't open fh to SEED.rp, $!";
open(ALN, $aln) or die "Couldn't open fh to $aln, $!"; 
my ($total, $keep)=(0, 0);
while(<ALN>) {
  if(/^(\S+)\.(\d+)\/\d+-\d+/) {  #B5W3F0.1/12-152
    my ($acc, $ver) = ($1, $2);
    $total++;

    $sth->execute($acc, $ver) or die "Couldn't execute statement ".$sth->errstr."\n";
    my $inPfamseq=$sth->fetchrow;

    if($inPfamseq) {
      print OUT $_;
      $keep++;
    }
    else {
      print STDERR "$acc.$ver is not in RP, discarding\n";
    }
  }
  else {
    print OUT $_; #GF lines for nested
  }
}
close ALN;
close OUT;
$sth->finish;
$dbh->disconnect();

print STDERR "$keep/$total sequences are in RP\n";


sub help {
  print<<EOF;

A script to remove sequences that are not in the current pfamseq set
from the SEED alignment. The input for the program is a file in the 
current working directory called 'SEED'. The output is a file called 
SEED.rp which contains the sequences from the SEED alignment that are 
in pfamseq. The script will report how many sequences have been kept
from the original SEED alignment.

usage: 

$0

EOF

  exit;
}


