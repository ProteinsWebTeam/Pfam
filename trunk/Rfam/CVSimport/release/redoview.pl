#!/usr/local/bin/perl -w

use strict;
use lib '/pfam/db/Rfam/scripts/Modules';
use Rfam;

my $file = shift;
my @accs;

if( $file ) {
    open( F, $file ) or die;
    while(<F>) {
	if( /^(\S+)\s*/ ) {
	    push( @accs, $1 );
	}
    }
}
else {
    @accs = &Rfam::get_allaccs();
}

chdir "$Rfam::current_dir" or die;

foreach my $acc ( @accs ) {
    my $id = Rfam::acc2id( $acc );
    print "Doing $id ....\n";
    system "$Rfam::scripts_dir/rfamrcs/makerfamview.pl $id" and die;
}
