#!/usr/local/bin/perl -w

use strict;
use lib '/nfs/disk56/sgj/pfam/scripts/rfam/scripts/Modules';
use Rfam;
use RfamRCS;

foreach my $acc ( Rfam->get_allaccs() ) {
    my $id = Rfam::acc2id( $acc );
    if( RfamRCS::view_file_errors( $id ) ) {
	warn "$acc: found errors with viewfiles\n";
    }
}

    
