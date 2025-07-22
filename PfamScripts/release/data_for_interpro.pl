#!/usr/bin/env perl

#obtain HMMs, SEEDs and CLANDESCs from pfam for interpro

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );

my @hmmrs = $pfamDB->getSchema->resultset('PfamAHmm')->search( { },
    { order_by => { -asc => 'pfama_acc' } });


#retrieve HMMs in uncompressed format

print "Retrieving HMMs\n";
open (HMM, ">pfam_a.hmm") or die "Can't open file to write\n";

foreach my $hmmrow (@hmmrs){
    my $hmm = $hmmrow->hmm;
    print HMM $hmm;
}

close HMM;

#retrieve CLANDESC stockholm files in zip format

print "Retrieving CLANDESC stockholm files";

my @clanrs = $pfamDB->getSchema->resultset('ClanAlignmentAndRelationship')->search( { },
    { order_by => { -asc => 'clan_acc' } });

open (CLANZIP, ">pfam_clans.gz") or die "Can't open file to write\n";
foreach my $clanrow (@clanrs){
    my $desczip = $clanrow->stockholm;
    print CLANZIP $desczip;

}

close CLANZIP;

system("gzip -d pfam_clans.gz");

#retrieve SEED stockholm files in zip format

print "Retrieving SEED stockholm files";

my @seedrs = $pfamDB->getSchema->resultset('AlignmentAndTree')->search( { type => 'seed'},
    { order_by => { -asc => 'pfama_acc' } } );

open (SEEDZIP, ">pfam_a.seed.gz") or die "Can't open file to write\n";

foreach my $seedrow (@seedrs){
    my $seedzip = $seedrow->alignment;
    print SEEDZIP $seedzip;

}

close SEEDZIP;

system("gzip -d pfam_a.seed.gz");


# make Pfam-A.hmm.dat file
print "Preparing Pfam-A.hmm.dat file\n";
my @AllFamData = $pfamDB->getSchema->resultset("PfamA") ->search( undef, { order_by => \'me.pfama_acc ASC' } );
my %acc2id;
foreach my $fam (@AllFamData) {
    $acc2id{ $fam->pfama_acc } = $fam->pfama_id;
}

open( PFAMSCAN, ">pfam_a.dat" ) or die "Can't open file to write\n";
foreach my $fam (@AllFamData) {
    print PFAMSCAN "# STOCKHOLM 1.0\n";
    print PFAMSCAN "#=GF ID   " . $fam->pfama_id . "\n";
    print PFAMSCAN "#=GF AC   " . $fam->pfama_acc . "." . $fam->version . "\n";
    print PFAMSCAN "#=GF DE   " . $fam->description . "\n";
    print PFAMSCAN "#=GF GA   "
      . $fam->sequence_ga . "; "
      . $fam->domain_ga . ";\n";
    print PFAMSCAN "#=GF TP   " . $fam->type->type . "\n";
    print PFAMSCAN "#=GF ML   " . $fam->model_length . "\n";

    my @nested =
      $pfamDB->getSchema->resultset("NestedLocation")
      ->search( { "pfama_acc" => $fam->pfama_acc } );

    foreach my $n (@nested) {
      my $nested_id = $acc2id{ $n->nested_pfama_acc->pfama_acc };
      print PFAMSCAN "#=GF NE   $nested_id\n";
    }

    my $clan = $pfamDB->getSchema->resultset("ClanMembership")->find(
      { pfama_acc => $fam->pfama_acc },
    );

    if ($clan) {
      print PFAMSCAN "#=GF CL   " . $clan->clan_acc->clan_acc . "\n";
    }
    print PFAMSCAN "//\n";
}
close(PFAMSCAN);

print "Completed!\n";