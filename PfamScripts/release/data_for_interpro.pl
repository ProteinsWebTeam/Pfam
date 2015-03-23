#!/usr/bin/env perl

#obtain HMMs, SEEDs and CLANDESCs from pfam for interpro

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );

my @hmmrs = $pfamDB->getSchema->resultset('PfamAHmm')->search( {} );


#retrieve HMMs in uncompressed format

print "Retrieving HMMs\n";
open (HMM, ">HMMs") or die "Can't open file to write\n";

foreach my $hmmrow (@hmmrs){
    my $hmm = $hmmrow->hmm;
    print HMM $hmm;
}

close HMM;

#retrieve CLANDESC stockholm files in zip format

print "Retrieving CLANDESC stockholm files";

my @clanrs = $pfamDB->getSchema->resultset('ClanAlignmentAndRelationship')->search( {} );

open (CLANZIP, ">clandesc.gz") or die "Can't open file to write\n";
foreach my $clanrow (@clanrs){
    my $desczip = $clanrow->stockholm;
    print CLANZIP $desczip;

}

close CLANZIP;

#retrieve SEED stockholm files in zip format

print "Retrieving SEED stockholm files";

my @seedrs = $pfamDB->getSchema->resultset('AlignmentAndTree')->search( { type => 'seed'} );

open (SEEDZIP, ">seed.gz") or die "Can't open file to write\n";

foreach my $seedrow (@seedrs){
    my $seedzip = $seedrow->alignment;
    print SEEDZIP $seedzip;

}

close SEEDZIP;
