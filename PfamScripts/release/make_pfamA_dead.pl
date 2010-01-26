#! /software/bin/perl -w

#Script to generate the Pfam-A.dead flatfile from rdb

use strict;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;
use Getopt::Long;

use Text::Wrap;


$Text::Wrap::columns = 70;

my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );


my @dead = $pfamDB->getSchema
                      ->resultset('DeadFamilies')
                      ->search({},);

foreach my $row (@dead) {
    print "# STOCKHOLM 1.0\n";

    print "#=GF ID   " . $row->pfama_id . "\n";
    print "#=GF AC   " . $row->pfama_acc . "\n";
    print "#=GF KL   This family has been killed\n";
    print "#=GF FW   " . $row->forward_to . "\n";

    my $comment = $row->comment ;
    if($comment =~ /^\s+(.+)$/) {
	$comment = $1;
    }
    print wrap("#=GF CC   ", "#=GF CC   ", $comment) . "\n";


    print "//\n";
}
