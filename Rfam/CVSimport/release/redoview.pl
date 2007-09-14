#!/software/bin/perl -w

use strict;
use Rfam;

my @accs;

if( @ARGV ) {
    @accs = @ARGV;
}
else {
    my $db = Rfam::default_db();
    @accs = $db->get_allacc();
}

foreach my $acc ( @accs ) {
    print "Doing $acc ....\n";
    system "$Rfam::view_maker -n $acc" and die;
}
