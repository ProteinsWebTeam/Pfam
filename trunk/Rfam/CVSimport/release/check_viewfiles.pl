#!/usr/local/bin/perl -w

use strict;
use lib '/pfam/db/Rfam/scripts/Modules';
use Rfam;
use RfamRCS;

foreach my $acc ( Rfam->get_allaccs() ) {
    my $id = Rfam::acc2id( $acc );
    if( RfamRCS::view_file_errors( $id ) ) {
	print "$acc: found errors with viewfiles\n";
    }
}

    
