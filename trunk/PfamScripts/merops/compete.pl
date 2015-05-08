#!/usr/bin/env perl

use strict;
use warnings;
use DBI;

my $dir = ".";
opendir(DIR, $dir) or die "Couldn't open directory '$dir', $!";
my @families = sort grep { ! /^\./ } readdir(DIR);

#Read clan data
my %clan;
clan(\%clan);

#Make a regions file that contains the regions from all families
my %regions;
my $regFile="regions.txt";
open(REG, ">$regFile") or die "Couldn't open fh to $regFile, $!"; 
foreach my $fam (@families) {
  next unless (-d $fam);
  print STDERR "$fam\n";
  
  my $scores = "$fam/hmmsearchTbl";
  open(TBL, $scores) or die "Couldn't open fh to $scores, $!";
  while(<TBL>) {
    next if(/^#/);
    #                                                              --- full sequence --- -------------- this domain -------------   hmm coord   ali coord   env coord
    # target name   accession   tlen query name  accession   qlen   E-value  score  bias   #  of  c-Evalue  i-Evalue  score  bias  from    to  from    to  from    to  acc description of target
    my @line = split(/\s+/, $_);
    my ($acc, $evalue, $bits, $ali_st, $ali_en, $env_st, $env_en) = ($line[0], $line[12], $line[13], $line[17], $line[18], $line[19], $line[20]);
    if($acc =~ /[sp|tr]\|(\S+)\|/) {
      $acc=$1;
    }
    print REG "$acc\t$fam\t$ali_st\t$ali_en\t$env_st\t$env_en\t$bits\t$evalue\n";
  }
  close TBL;
}
close REG;

#Sort the regions file, first by acc, then by bit score
my $sortedRegFile="sortedRegFile.txt";
system("sort -k1,1 -k7nr,7 $regFile > $sortedRegFile") and die "Couldn't sort $regFile, $!";
unlink($regFile);

#Go through regions and find out which will be outcompeted
print STDERR "Going through regions\n";
my (%delete, $lastAcc, %done);
open(DIFF, ">diffClans.txt") or die "Couldn't open fh to diffClans.txt";
open(DSEQ, ">diffClansSeq.txt") or die "Couldn't open fh to diffClansSeq.txt";
open(REG, $sortedRegFile) or die "Couldn't open fh to $sortedRegFile, $!";
while(<REG>) {
  my ($acc, $fam, $ali_st, $ali_en, $env_st, $env_en, $bits) = split(/\s+/, $_);
  $lastAcc=$acc unless($lastAcc);
 
  #Empty the regions hash if we've moved to the next seq
  if($lastAcc ne $acc) {    
    %regions=();
    $lastAcc=$acc;
  }

  #Check it doesn't overlap to anything with higher bit score
  my $overlap;
  if(exists($regions{$acc})) {
    foreach my $reg (@{$regions{$acc}}) {
      next if($reg->{fam} eq $fam);
      unless($reg->{ali_st} > $ali_en or $reg->{ali_en} < $ali_st) { #Unless it doesn't overlap
  
        $overlap=1;

        #See if the two families are in the same clan, if not lets report it
        my $f1 = name($fam);
        my $f2 = name($reg->{fam});
        unless($clan{$f1} eq $clan{$f2}) {
          print DIFF "$f1 ($clan{$f1}) and $f2 ($clan{$f2}) are in different clans\n" unless(exists($done{"$f1:$f2"}));
          print DSEQ "$fam $acc/$ali_st-$ali_en ($acc/$env_st-$env_en $bits bits) overlaps with ".$reg->{fam}." $acc/".$reg->{ali_st}."-".$reg->{ali_en}." ($acc/".$reg->{env_st}."-".$reg->{env_en}." ".$reg->{score}." bits)\n";
          $done{"$f1:$f2"}=1;
        }
        $delete{$fam}.= "$fam $acc/$ali_st-$ali_en ($acc/$env_st-$env_en, $bits bits) overlaps with ".$reg->{fam}." $acc/".$reg->{ali_st}."-".$reg->{ali_en}." ($acc/".$reg->{env_st}."-".$reg->{env_en}.", ".$reg->{score}." bits)\n";
        last;
      }
    }
  }
  #If no overlap, then add it to the hash
  unless($overlap) {
    push(@{$regions{$acc}}, { fam => $fam, ali_st => $ali_st, ali_en => $ali_en, env_st => $env_st, env_en => $env_en, score => $bits});
  }
}
close REG;
close DIFF;
close DSEQ;

#Print outcompeted seq to the family dirs
foreach my $fam (keys %delete) {
  my $outfile="$fam/outcompeted.txt";
  open(OUT, ">$outfile") or die "Couldn't open fh to $outfile, $!";
  print OUT $delete{$fam};
  close OUT;
}

sub name {
  my $fam = shift;

  #Extract the family name from subfamily 
  if($fam =~ /([A-Z]\d+)[A-Z]/) { #eg M50A becomes M50
    $fam = $1;
  }
  return $fam;
}


sub clan {

  my ($hash) = @_;

  #Get mapping of families to clan
  my $dbh = DBI->connect("dbi:mysql:database=meropsweb;host=mysql-merops-curation:port=4408", "admin", "gCox9MH5");
  my $sth=$dbh->prepare("select family, clan from family");
  $sth->execute() or die "Couldn't execute statement ".$sth->errstr."\n";
  my ($fam, $clan);
  $sth->bind_columns(\$fam, \$clan);
  while ($sth->fetch()) { 
    $hash->{$fam}=$clan;
  }
}
