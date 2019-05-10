#!/usr/bin/env perl

use strict;
use warnings;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;

#Script to generate jaccard index and jacard containment values
#for each pair of families in mgnifam. This script prints out a 
#file to the cwd of families that have midpoint overlaps, along 
#with the jaccard index and jaccard containment values


#Get database connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

unless($pfamDB->{database} eq "mgnifam") {
  die "Need to use the config for mgnifam\n";
}


#Get all the mgnifams
my @mgnifams = $pfamDB->getSchema->resultset('Mgnifam')->search({});


#Loop through and compare all against all
my (%done, $header);
my $outfile = "";
open(OUT, ">$outfile") or die "Couldn't open fh to $outfile, $!";
foreach my $mgnifam1 (@mgnifams) {

  #Gell all regions for mgnifam1
  my @regions1 = $pfamDB->getSchema->resultset("MgnifamRegFull")->search({ mgnifam_acc => $mgnifam1->mgnifam_acc });

  #Put regions in a hash so it is easier to compare
  my %regs;
  foreach my $reg1 (@regions1) {
    my $midpoint1 = ($reg1->seq_end - $reg1->seq_start) / 2 + $reg1->seq_start;
    push(@{$regs{$reg1->pfamseq_acc}}, { start => $reg1->seq_start, end => $reg1->seq_end, midpoint => $midpoint1 });
  }

  #Count the regions in family1
  my $count1 = @regions1;

  
  foreach my $mgnifam2 (@mgnifams) {
    next if($mgnifam1->mgnifam_acc eq $mgnifam2->mgnifam_acc); #Don't compare with itself

    my $done1 = $mgnifam1->mgnifam_acc.":".$mgnifam2->mgnifam_acc; 
    my $done2 = $mgnifam2->mgnifam_acc.".".$mgnifam1->mgnifam_acc;

    next if(exists($done{$done1})); #Already done this pair

    $done{$done1}=1;
    $done{$done2}=1;

    #Get all regions for mgnifam2
    my @regions2 = $pfamDB->getSchema->resultset("MgnifamRegFull")->search({ mgnifam_acc => $mgnifam2->mgnifam_acc });

    #Look for midpoint overlaps
    my $intersection=0;
    foreach my $reg2 (@regions2) {
      my ($acc, $start2, $end2) = ($reg2->pfamseq_acc, $reg2->seq_start, $reg2->seq_end);
      my $midpoint2 = ($end2 - $start2)/2 + $start2;

      if(exists($regs{$acc})) {
        foreach my $reg1 (@{$regs{$acc}}) {
          if($midpoint2 >= $reg1->{start} and $midpoint2 <= $reg1->{end}) {
            $intersection++;
          }
          elsif($reg1->{midpoint} >= $start2 and $reg1->{midpoint} <= $end2) {
            $intersection++;
          }
        }
      }
    }

    #Count the regions in family2
    my $count2=@regions2;

    #Report if there are overlaps
    if($intersection) {
      unless($header) {
        $header=1;
        print OUT "#Family1\tFamily2\tji\tjc_family1\tjc_family2\n";
      }

      my $union = $count1 + $count2 - $intersection;

      my $ji = $intersection/$union;

      my $jc1 = $intersection/$count1;
      my $jc2 = $intersection/$count2;

      print OUT $mgnifam1->mgnifam_acc ."\t". $mgnifam2->mgnifam_acc ."\t$ji\t$jc1\t$jc2\n";
    }
  }
}
close OUT;

if($header) {
  print STDERR "Error: There are overlapping families in the mgnifam database, see $outfile for details\n";
}
else {
  print STDERR "No families overlap, database passes jaccard qc\n";
}
