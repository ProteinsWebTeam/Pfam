#! /usr/bin/env perl 

#Script to make a pfamseq file for the hmmer pipeline
#The file is the same as pfamseq, but additionally has the organism name in the header
#Uses <1gb of memory

use strict;
use warnings;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

my $offset = 0;
my $n = 1;
my $total = 0;

my $outfile="pfamseq_hmmer.fa";
open (F, ">$outfile") or die "Couldn't open fh to $outfile, $!";
while (1){
    print "Querying database.... chunk $n offset $offset ";

    my $st = $dbh->prepare("select pfamseq_acc, seq_version, pfamseq_id, description, species, sequence from pfamseq limit 1000000 offset $offset") or die "Failed to prepare statement:".$dbh->errstr."\n";

    $st->execute() or die "Couldn't execute statement ".$st->errstr."\n";

    my $rowno = $st->rows;

    unless($rowno) {
      print "$total rows retrieved\n";
      last;
    }

    my ($pfamseq_acc, $seq_version, $pfamseq_id, $description, $species, $sequence);  
    
    $st->bind_columns(\$pfamseq_acc, \$seq_version, \$pfamseq_id, \$description, \$species, \$sequence);

    while ($st->fetch()) {
      print F ">$pfamseq_acc.$seq_version $pfamseq_id $description OS=$species\n$sequence\n";
    }

    $offset += 1000000;
    $total += $rowno;
    $n++;
    print "$total rows retrieved\n";

}
close F;
$dbh->disconnect;
