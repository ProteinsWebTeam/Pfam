#!/usr/bin/env perl
# 
# writeAnnotatedSeed.pl:  Generates annotated SEED files for use as release flatfiles
#
# 
use strict;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::Family::CM;
use Bio::Rfam::Config;
use Bio::Rfam::SVN::Client;


my $config = Bio::Rfam::Config->new;
my $client = Bio::Rfam::SVN::Client->new({config => $config});
my $family = $ARGV[0];

my $familyIO = Bio::Rfam::FamilyIO->new;

my $familyObj = $familyIO->loadRfamFromSVN($family, $client);
my $msa = $familyObj->SEED;
$msa->write_msa("$family.seed","pfam");
my $DESC = $familyObj->DESC;
$familyIO->writeDESC($DESC);
my $num_full = $msa->nseq;

my $outfile = "$family";

open (OUTFILE, ">>", $family);
print OUTFILE "# STOCKHOLM 1.0\n\n";
open (DESC, "<", "DESC");

while (<DESC>) {
  chomp;
  print OUTFILE "#=GF ";
  print OUTFILE "$_\n";
}
print OUTFILE "#=GF SQ   ";
print OUTFILE "$num_full\n\n";
close (DESC);

open (SEED, "<", "$family.seed");
while (<SEED>) {
  chomp;
  my $line = $_;
  if ($line =~ /#=GC/ || $line !~ /#/) {
    print OUTFILE "$line\n";
  } else {
    next;
  }
}
close (SEED);
close (OUTFILE);
unlink "$family.seed";
unlink "DESC";
