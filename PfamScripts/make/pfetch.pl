#!/usr/bin/env perl

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#Script to fetch protein sequence

my $acc = shift;
unless($acc) {
    help();
}

my $config = Bio::Pfam::Config->new;
my $seq_db = $config->{uniprot}->{location} . "/uniprot";

if($acc =~ /^(\S+\.\d+)$/) { #has version number, eg P15498.4
    my $acc_ver = $1;
    get_seq($seq_db, $acc_ver);
    exit 0;
}


my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;


my $acc_ver;
if($acc =~ /^(\S+_\S+)/) { #sequence id, eg VAV_HUMAN. Need to get the accession and version number from db
    my $id = $1;
    my $sth = $dbh->prepare("select uniprot_acc, seq_version from uniprot where uniprot_id = '$id'");
    $sth->execute() or die "Couldn't execute statement ".$sth->errstr."\n";
    my ($a, $ver) = $sth->fetchrow();
    $acc_ver = $a . ".". $ver; 
}
else { #sequence acc, without version, eg P15498. Need to get version number from db
    my $sth = $dbh->prepare("select uniprot_acc, seq_version from uniprot where uniprot_acc = '$acc'");
    $sth->execute() or die "Couldn't execute statement ".$sth->errstr."\n";
    my ($a, $ver) = $sth->fetchrow();
    $acc_ver = $a . ".". $ver; 
}
get_seq($seq_db, $acc_ver);

sub get_seq {
    my ($seq_db, $acc_ver) = @_;
    system("esl-sfetch $seq_db $acc_ver") and die "Unable to retrieve $acc_ver from $seq_db, $!\n";
}
    
sub help {
  print<<EOF;

A script to retrieve the protein sequence for a sequence that is the 
current snapshot of UniprotKB. The sequence will be printed to the 
screen in FASTA format.

This script is a basic version of a previous pfetch script that 
was used at Sanger. It uses the esl-sfetch program on our local
snapshot of UniProtKB.

Usage:

  pfetch <protein accession or id>

Examples:
  pfetch P15498.4
  pfetch P15498
  pfetch VAV_HUMAN

EOF

  exit;
}
