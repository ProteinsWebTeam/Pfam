#!/usr/local/bin/perl -w

use strict;
use lib '/pfam/db/Rfam/scripts/Modules';
use Rfam;

my @accs;

if( @ARGV ) {
    @accs = @ARGV;
}
else {
    @accs = &Rfam::get_allaccs();
}

foreach my $acc ( @accs ) {
    print "Doing $acc ....\n";
    system "$view_maker $acc" and die;
}
