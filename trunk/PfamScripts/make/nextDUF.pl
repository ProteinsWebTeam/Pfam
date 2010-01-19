#!/software/bin/perl

use strict;
use warnings;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
unless($pfamDB){
  die "Could not connect to pfam database\n";  
}

#Need to get a list of all pfam ids;
my @dufNos;
my $pfams    = $pfamDB->getAllPfamFamilyDataLike('DUF%');
my $deadFams = $pfamDB->getAllDeadFamilyData; 
foreach my $fam (@{$pfams}, @{$deadFams}){
  if($fam->pfama_id =~ /^DUF(\d+)$/){
    push(@dufNos, $1); 
  }
}

my @dufNosSort = sort {$b <=> $a} @dufNos;

my $no = $dufNosSort[0] + 1;

print "\n The next available DUF number is: DUF$no\n\n";
