#!/usr/bin/env perl

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;


#Script to fetch protein sequence
use Getopt::Long;

my $start = 0;
my $end = 0;
GetOptions('-s=i' => \$start,
           '-e=i' => \$end);

my $acc = shift;
unless($acc) {
    help();
}

my $config = Bio::Pfam::Config->new;
my $seq_db = $config->{uniprot}->{location} . "/uniprot";

if($acc =~ /^(\S+\.\d+)$/) { #has version number, eg P15498.4
    my $acc_ver = $1;
    get_seq($seq_db, $acc_ver, $start, $end);
    exit 0;
}

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;


my $acc_ver = $acc;
if($acc =~ /^(\S+_\S+)/) { #sequence id, eg VAV_HUMAN. Need to get the accession and version number from db
    my $id = $1;
    my $sth = $dbh->prepare("select uniprot_acc, seq_version from uniprot where uniprot_id = '$id'");
    $sth->execute() or die "Couldn't execute statement ".$sth->errstr."\n";
    my ($a, $ver) = $sth->fetchrow();
    unless($a and $ver) {
        warn "$acc was not found in the current snapshot of uniprot\n";
        exit 1;
    }
    $acc_ver = $a . ".". $ver; 
}
else { #sequence acc, without version, eg P15498. Need to get version number from db
    my $sth = $dbh->prepare("select uniprot_acc, seq_version from uniprot where uniprot_acc = '$acc'");
    $sth->execute() or die "Couldn't execute statement ".$sth->errstr."\n";
    my ($a, $ver) = $sth->fetchrow();
    unless($a and $ver) {
        warn "$acc was not found in the current snapshot of uniprot\n";
        exit 1;
    }       
    $acc_ver = $a . ".". $ver; 
}
get_seq($seq_db, $acc_ver, $start, $end);



sub get_seq {
    my ($seq_db, $acc_ver, $st, $en) = @_;

    if($st or $en) {
        unless($st and $en) {
            die "Need to specify both start and end co-ordinates\n";
        }
        my $c = $st . ".." . $en;
        system("esl-sfetch -c $c $seq_db $acc_ver") and die "Unable to retrieve $acc_ver from $seq_db, $!\n";
    }
    else {
        system("esl-sfetch $seq_db $acc_ver") and die "Unable to retrieve $acc_ver from $seq_db, $!\n";
    }
}
    
sub help {
  print<<EOF;

A script to retrieve the amino acide sequence for a protein that is the
current snapshot of UniprotKB. The sequence will be printed to the
screen in FASTA format.

This script uses the esl-sfetch program, and is a basic version of
a previous pfetch script that was used at Sanger.

Usage:

  pfetch <protein accession or id>

Options:
    -s <start>   : Specify start co-ordinate
    -e <end>     : Specify end co-ordinate

Examples:
  pfetch P15498.4
  pfetch P15498
  pfetch VAV_HUMAN
  pfetch -s 1 -e 100 P15498

EOF

  exit;
}
