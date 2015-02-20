#!/usr/bin/env perl

use strict;
use warnings;
use Bio::Range;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;

#Script to check for overlaps between alignment co-ordinates in the pfamA_reg_full_significant table in pfam_live
#Clans are competed before checking for overlaps
#Overlaps are only reported for reference proteome sequences
#Overlaps are filtered according to parameters in config file
#The script will dump the regions from pfamA_reg_full_significant in the cwd
#Output is a file called overlaps in the cwd and format is:
#L: Sequence [G3W0U7] overlap LRRNT PF01462/29-57 (PF01462/28-57, 23.40 bits) FULL with LRR_8 PF13855/45-94 (PF13855/42-94, 30.30 bits) FULL
#S: (2/54, 3.7%) Sequence [G3W0U7] overlap LRRNT PF01462/29-57 (PF01462/28-57, 23.40 bits) FULL with LRR_8 PF13855/56-94 (PF13855/50-94, 30.30 bits) FULL
#L = long overlap
#S = short overlap, followed by the proportion of members that have an overlap in brackets


#Going to write some files in cwd, make sure we don't overwrite anything
my $regFull="regFull.dat";
my $sortedRegFull="sortedRegFull.dat";
my $outfile = "overlaps";
if(-s $regFull or -s $sortedRegFull or -s $outfile) {
  die "$regFull and/or $sortedRegFull and/or $outfile already exists in cwd";
}

#Get db connection
my $config = Bio::Pfam::Config->new;
my $pfamDBAdmin = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } ); #Need admin user to dump pfamA_reg_full_significant 
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;

# Filter the overlaps according to paramaters found in config file
my $lengthLimit = $config->sequenceOverlapRule; #Proportion of lowest scoring match length that is allowed to overlap
my $numberLimit = $config->familyOverlapRule; #% of ALIGN regions allowed to overlap
print STDERR "Short overlap is defined as <$lengthLimit"."% of the length of the lowest scoring match\n<$numberLimit"."% of the family is permitted to have short overlaps\n";
  
#Store ref proteome accessions in a hash
print STDERR "Getting reference proteome accessions from database\n";
my $sth1=$dbh->prepare("select pfamseq_acc from pfamseq where ref_proteome=1");
$sth1->execute() or die "Couldn't execute statement ".$sth1->errstr."\n";
my ($refProtAcc, %refProt);
$sth1->bind_columns(\$refProtAcc);
while ($sth1->fetch()) {
  $refProt{$refProtAcc}=1;
}
$sth1->finish;

#Get all regions from pfamA_reg_full_significant
print STDERR "Getting regions from pfamA_reg_full_significant\n";
my $command = "mysql -h ".$pfamDBAdmin->{host}." -u ".$pfamDBAdmin->{user}." -p". $pfamDBAdmin->{password}." -P ".$pfamDBAdmin->{port}." ".$pfamDBAdmin->{database}." --quick -e \"select pfamA_acc, pfamseq_acc, seq_start, seq_end, ali_start, ali_end, domain_bits_score, domain_evalue_score from pfamA_reg_full_significant\" > $regFull";
system("$command");
die "Failed to get regions from the database" unless(-s $regFull);

#Sort the file by pfamseq_acc first, E-value second
print STDERR "Sorting regions data\n";
system("sort -k2,2 -k8,8g $regFull > $sortedRegFull"); 
die "Failed to sort region file" unless(-s $sortedRegFull);
unless($sortedRegFull and (-s $sortedRegFull eq -s $regFull)) {
  die "Failed to sort region file, $sortedRegFull is not the same size as $regFull";
}
unlink $regFull;

#Get clan data
my %clan;
getClanData($pfamDB, \%clan);

#Get nested data
#If family is in a clan, store at clan level, otherwise store at family level
my %nested;
getNestedData($pfamDB, \%nested, \%clan);

#Get pfamA_ids
my %pfamA_id;
getPfamAId($pfamDB, \%pfamA_id);

#Go through regions file and look for overlaps
print STDERR "Looking for overlaps in $sortedRegFull\n";
my ($lastAcc, @regions, $seqAcc, $st, $en, $ali_st, $ali_en, $pfamA, $bits);
my (%shortOverlaps, %longOverlaps);
open(REG, $sortedRegFull) or die "Couldn't open fh to $sortedRegFull, $!";
while(<REG>) {
  next if(/^pfamA_acc/);
  
  if(/^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/) {
    ($pfamA, $seqAcc, $st, $en, $ali_st, $ali_en, $bits) = ($1, $2, $3, $4, $5, $6, $7);

    next unless(exists($refProt{$seqAcc})); #Only consider reference proteome sequences

    my $clan=$clan{$pfamA}; 
    
    if(!$lastAcc) {
      $lastAcc=$seqAcc;
    }
    elsif($lastAcc ne $seqAcc) {
      findOverlaps(\@regions, $lastAcc, \%clan, \%nested, \%shortOverlaps, \%longOverlaps, $lengthLimit);
      @regions = ();
      $lastAcc=$seqAcc;
    }

    my $overlap;
    if(@regions and $clan) { #If there are already region(s) in the @array, and family is in a clan then need to check for overlapping clan hits
      #Do not add overlapping regions within the same clan to the array (ie compete the clan)
      #Regions file is sorted by E-value so lowest E-value region is always added to @regions first
      foreach my $region (@regions) {
	if($region->{clan} and $region->{clan} eq $clan) {
	  if($region->{ali_end} < $ali_st or $ali_en < $region->{ali_start}) { #No overlap
	    next;
	  }
	  elsif($region->{pfamA} eq $pfamA) { #Internal overlap, so doesn't need competing
	    next;
	  }
	  else { #Overlaps to another family in the same clan so don't add to array
	    $overlap=1;
	    last;
	  }
	}
      }
    }
    unless($overlap) {
      push (@regions, { start => $st, end => $en, ali_start => $ali_st, ali_end => $ali_en, pfamA => $pfamA, pfamA_id => $pfamA_id{$pfamA}, clan => $clan, bits => $bits });
    }
  }
  else {
    die "Unrecognised format: $_";
  }
}
close REG;
#Need to do the last accession in the file
findOverlaps(\@regions, $seqAcc, \%clan, \%nested, \%shortOverlaps, \%longOverlaps, $lengthLimit);
print STDERR "Read all regions and calculated the overlaps\n";

#Now go through all families and print overlaps
open(OUT, ">$outfile") or die "Couldn't open fh to $outfile, $!";
my (%shortOverlapFam, %longOverlapFam);
my $sth2=$dbh->prepare("select count(*) from pfamA_reg_full_significant where pfamA_acc=?"); #Query for total regions in family
foreach my $pfamA (sort keys %longOverlaps) {
  foreach my $overlap (keys %{$longOverlaps{$pfamA}}) {
    print OUT "L: $overlap\n";
    $longOverlapFam{$pfamA}=1;
  }

  #Only report short overlaps if they go above the threshold set in config file
  my $numShortOverlaps=keys %{$shortOverlaps{$pfamA}};
  $sth2->execute($pfamA) or die "Couldn't execute statement ".$sth2->errstr."\n";
  my $famSize = $sth2->fetchrow;
  die "Couldn't retrieve family size for [$pfamA]" unless($famSize);

  my $propShortOverlaps=$numShortOverlaps/$famSize*100;
  if($propShortOverlaps >= $numberLimit) {
    $propShortOverlaps = sprintf("%.1f", $propShortOverlaps);
    foreach my $overlap (keys %{$shortOverlaps{$pfamA}}) {
      print OUT "S: ($numShortOverlaps/$famSize, $propShortOverlaps"."%) $overlap\n";
      $shortOverlapFam{$pfamA}=1;
    }
  }    
}

foreach my $pfamA (sort keys %shortOverlaps) {
  next if(exists($longOverlaps{$pfamA})); #Printed this family already
  my $numShortOverlaps=keys %{$shortOverlaps{$pfamA}};
  $sth2->execute($pfamA) or die "Couldn't execute statement ".$sth2->errstr."\n";
  my $famSize = $sth2->fetchrow;
  die "Couldn't retrieve family size for [$pfamA]" unless($famSize);

  my $propShortOverlaps=$numShortOverlaps/$famSize*100;
  if($propShortOverlaps >= $numberLimit) {
    $propShortOverlaps = sprintf("%.1f", $propShortOverlaps);
    foreach my $overlap (keys %{$shortOverlaps{$pfamA}}) {
      print OUT "S: ($numShortOverlaps/$famSize, $propShortOverlaps" ."%) $overlap\n";
      $shortOverlapFam{$pfamA}=1;
    }
  }    
}
$sth2->finish;
$dbh->disconnect();

my $totLongOvFams=keys %longOverlapFam;
my $totShortOvFams=keys %shortOverlapFam;
print STDERR "$totLongOvFams families have long overlaps\n$totShortOvFams families have short overlaps\n";
print OUT "$totLongOvFams families have long overlaps\n$totShortOvFams families have short overlaps\n";
close OUT;

sub findOverlaps {
  my ($regions, $acc, $clan, $nested, $shortOverlaps, $longOverlaps, $lengthLimit) = @_;


  foreach(my $i=0; $i<@$regions; $i++) { #Compare all against all in array
    foreach (my $j=$i+1; $j<@$regions; $j++) {
      my $reg1=$regions->[$i];
      my $reg2=$regions->[$j];

      next if($reg1->{pfamA} eq $reg2->{pfamA}); #Ignore internal overlaps
      
      my $clan1=$clan->{$reg1->{pfamA}};
      my $clan2=$clan->{$reg2->{pfamA}};

      $clan1=$reg1->{pfamA} unless($clan1); #Nested data is stored at family level if not in a clan
      $clan2=$reg2->{pfamA} unless($clan2);
      next if(exists($nested{$clan1}{$clan2})); #One family is nested in the other, so not an overlap
      
      next if($reg1->{ali_end} < $reg2->{ali_start} or $reg2->{ali_end} < $reg1->{ali_start}); #No overlap
      
      #Calculate length of overlap
      my $regionA = new Bio::Range( -start => $reg1->{ali_start}, -end => $reg1->{ali_end}, -strand => +1 );
      my $regionB = new Bio::Range( -start => $reg2->{ali_start}, -end => $reg2->{ali_end}, -strand => +1 );
      my ($start, $end, $strand) = $regionA->intersection($regionB);
      my $overlapLength = ($end - $start) + 1;

      #Store overlap as a short or long overlap, lowest bit score family gets the overlap and is printed first
      if($reg1->{bits} < $reg2->{bits}) {
	my $overlapPerc = $overlapLength / $regionA->length * 100 ;
	my $overlapLine = "Sequence [$acc] overlap ". $reg1->{pfamA_id}." ".$reg1->{pfamA}."/".$reg1->{ali_start}."-".$reg1->{ali_end}." (".$reg1->{pfamA}."/".$reg1->{start}."-".$reg1->{end}.", ". $reg1->{bits}." bits) FULL with ".$reg2->{pfamA_id}." ".$reg2->{pfamA}."/".$reg2->{ali_start}."-".$reg2->{ali_end}." (". $reg2->{pfamA}."/".$reg2->{start}."-".$reg2->{end}.", ".$reg2->{bits}." bits) FULL";
	if($overlapPerc < $lengthLimit) { # Short overlaps that are allowed, because the overlap length is less than $lengthLimit %
	  $shortOverlaps{$reg1->{pfamA}}{$overlapLine}=1;
	}
	else {
	  $longOverlaps{$reg1->{pfamA}}{$overlapLine}=1;
	}
      }
      else {
	my $overlapPerc = $overlapLength / $regionB->length * 100 ;
	my $overlapLine = "Sequence [$acc] overlap ". $reg2->{pfamA_id}." ".$reg2->{pfamA}."/".$reg2->{ali_start}."-".$reg2->{ali_end}." (".$reg2->{pfamA}."/".$reg2->{start}."-".$reg2->{end}.", ".$reg2->{bits}." bits) FULL with ".$reg1->{pfamA_id}." ".$reg1->{pfamA}."/".$reg1->{ali_start}."-".$reg1->{ali_end}." (".$reg1->{pfamA}."/".$reg1->{start}."-".$reg1->{end}.", ". $reg1->{bits}." bits) FULL";
	if($overlapPerc < $lengthLimit) { # Short overlaps that are allowed, because the overlap length is less than $lengthLimit %
	  $shortOverlaps->{$reg2->{pfamA}}->{$overlapLine}=1;
	}
	else {
	  $longOverlaps->{$reg2->{pfamA}}->{$overlapLine}=1;
	}
      }
    }
  }
}

sub getClanData {
  my ($pfamDB, $clan) = (@_);

  print STDERR "Getting clan data from rdb\n";
  my @allClans = $pfamDB->getSchema
    ->resultset('Clan')
    ->search({},); 

  foreach my $c (@allClans) {
    my @clanData = $pfamDB->getSchema->resultset("ClanMembership")->search(
      { "clan_acc.clan_acc" => $c->clan_acc },
      {
        join     => [qw/clan_acc pfama_acc/],
        prefetch => [qw/clan_acc pfama_acc/]
      }
    );

    foreach my $pfamA (@clanData)  {
      $clan->{$pfamA->pfama_acc->pfama_acc}=$pfamA->clan_acc->clan_acc;
    }

  }
}

sub getNestedData {
  my ($pfamDB, $nested, $clan) = (@_);

  print STDERR "Getting nested data from rdb\n";
  my @allNested = $pfamDB->getSchema
    ->resultset('NestedDomain')
    ->search({},);

  foreach my $pfamA (@allNested) {
    my $acc1= $pfamA->pfama_acc->pfama_acc;
    my $acc2= $pfamA->nests_pfama_acc->pfama_acc;

    #Store nested info at the clan level in in a clan, or at the family level if not in a clan
    my $clan1=$clan->{$acc1};
    my $clan2=$clan->{$acc2};

    $clan1=$acc1 unless($clan1);
    $clan2=$acc2 unless($clan2);

    $nested->{$clan1}->{$clan2}=1;
    $nested->{$clan2}->{$clan1}=1;
  }
}

sub getPfamAId {
  my ($pfamDB, $pfamA_id) = (@_);
  print STDERR "Getting pfamA_ids from rdb\n";

  my @allPfamA = $pfamDB->getSchema
    ->resultset('PfamA')
    ->search({},);

  foreach my $pfamA (@allPfamA) {
    $pfamA_id->{$pfamA->pfama_acc}=$pfamA->pfama_id;
  }
}
