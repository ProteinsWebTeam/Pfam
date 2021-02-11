#!/usr/bin/env perl

#Script to submit uniprot searches to farm
#Can be run on all families (-all), or on a single family (-pfamA <pfamA_acc>)
#The script estimates how much memory to request on the farm
#A handful (10 for uniprot29) will fail because insufficient memory was requested
#Re-run these using the -M option

use strict;
use warnings;
use POSIX qw(ceil);
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;
use DDP;

#Get user options and check they are sensible
my ($all, $acc, $clan, $memory_gb);
GetOptions('all' => \$all,
  'pfamA=s'  => \$acc,
  'clan=s' => \$clan,
  'M=i'  => \$memory_gb);

unless($all or $acc or $clan) {
  die "Need to specify whether to do all families (-all), a single family (-pfamA <pfamA_acc>) or a single clan (-clan <clan_acc>)\n";
}
if( ($all and $acc) or ($all and $clan) or ($acc and $clan)) {
  die "Can only choose one of -all, -acc and -clan\n";
}
if($all and $memory_gb) {
  die "Can't use -all and -M option together\n";
}

#Some families need more memory that is estimated by this script, so hard code them to request extra memory
my @extra_mem = qw(PF00005 PF00069 PF00072 PF00083 PF00106 PF00115 PF00501 PF00528 PF07690 PF07714 PF13561);
my %memory;
foreach my $f (@extra_mem) {
  if($f eq "PF00005" or $f eq "PF07690") { #These families need a lot more memory
    $memory{$f}=64;
  }
  else {
    $memory{$f}=32;
  }
}


#If user has defined how much memory to use, add this to hash
if($memory_gb) {
  if($clan) {
    $memory{$clan}=$memory_gb;
  }
  else {
    $memory{$acc}=$memory_gb;
  }
}

#Get database connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );

#Create array of families to work on
if($all) {

  #Set uniprot_competed to 0 for all families
  my $dbh = $pfamDB->getSchema->storage->dbh;
  my $st = $dbh->prepare("update clan set uniprot_competed=0");
  $st->execute() or die "Couldn't execute statement ".$st->errstr."\n";

  #First do the clan families
  my @clanData = $pfamDB->getSchema->resultset("ClanMembership")->search();

  my %clan_pfamA;
  my %clan_family;
  foreach my $c (@clanData) { 
    $clan_family{$c->pfama_acc->pfama_acc}=1;
    my $family = $pfamDB->getSchema->resultset('PfamA')->find({ pfama_acc => $c->pfama_acc->pfama_acc});
    push(@{$clan_pfamA{$c->clan_acc->clan_acc}}, $family);
  }
  foreach my $clan (sort keys %clan_pfamA) {
    uniprotSearch($clan_pfamA{$clan}, $clan, \%memory); #$clan_pfamA{$clan} is an array ref
  }


  #Then the non-clan families
  my @pfamA;
  my @all_pfamA=$pfamDB->getSchema->resultset('PfamA')->search();
  foreach my $p (@all_pfamA) {
    unless(exists($clan_family{$p->pfama_acc})) {
      push(@pfamA, $p);
    }
  }
  uniprotSearch(\@pfamA, "", \%memory);

}
elsif($clan) {
  my @clanData = $pfamDB->getSchema->resultset("ClanMembership")->search({"clan_acc" => $clan});

  my @clan_pfamA;
  foreach my $c (@clanData) { 
    my $family = $pfamDB->getSchema->resultset('PfamA')->find({ pfama_acc => $c->pfama_acc->pfama_acc});
    push(@clan_pfamA, $family);
  }
  uniprotSearch(\@clan_pfamA, $clan, \%memory);
}
elsif($acc) {
  my @pfamA=$pfamDB->getSchema->resultset('PfamA')->find({pfama_acc => $acc});
  uniprotSearch(\@pfamA, "", \%memory);
}

sub uniprotSearch {

  my ($families, $clan_acc, $memory) = @_;

  my ($job_name, $num_clan, $clan_group);
  if($clan_acc) {
    $job_name="uniprot_".$clan_acc;
    $num_clan=@$families;
    $clan_group='/Competeclan';
  }

  my $uniprot_search = "performUniprotSearch.pl";
  my $group='/Uniprotview';

  #Loop through each pfamA and run searches
  foreach my $pfamA (@$families) {
    my $pfamA_acc=$pfamA->pfama_acc;

    unless($clan_acc) {
      $job_name=$pfamA_acc;
    }

    #Estimate memory if not already defined
    if($clan and $memory->{$clan_acc}) {
      $memory_gb=$memory->{$clan_acc};
    }
    elsif($memory->{$pfamA_acc}) {
      $memory_gb=$memory{$pfamA_acc};
    }
    else {
        #Going to estimate how much memory will be needed using this equation:
        #(Model length L * 40000 (longest sequence) * 48 (number of bytes in the dp) * number of cpus) /1,000,000
        my $cpu=4;
        $memory_gb = ceil(($pfamA->model_length * 40000 * 48 * $cpu)/1000000000);
        $memory_gb+=2; #Add an extra 2gb as the formula underestimated memory for many families for Pfam 33.0

        if($clan_acc and $memory_gb < 16) {
            $memory_gb=16; #If it's in a clan, set it to at least 16Gb
        }
    }
    my $memory_mb=$memory_gb*1000;  
    
    #Submit to farm
    print STDERR "$pfamA_acc\n";
    my $queue = $config->{farm}->{lsf}->{queue};
    system("bsub -q $queue -J$job_name -o $pfamA_acc.log -M $memory_mb -R \"rusage[mem=$memory_mb]\" -g $group '$uniprot_search $pfamA_acc'");
  }

  if($clan_acc) {
    my $queue = $config->{farm}->{lsf}->{queue};
    my $clan_name="compete_".$clan_acc;
    system("bsub -q $queue -J$clan_name -o $clan_acc.log -M 16000 -R \"rusage[mem=16000]\" -g $clan_group -w 'done($job_name)' 'competeUniprotClan.pl -clan $clan_acc'"); 
  }
}
