#!/usr/local/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
}

use lib $rfam_mod_dir;
use strict;
use Rfam;

my $flatfile = shift;
my @accs;
if( $flatfile ) {
    my $acc;
    open( F, $flatfile ) or die;
    $/ = "//\n";
    while(<F>) {
	if( /\#=GF AC   (RF\d+)/ ) {
	    $acc = $1;
	}
	open( A, ">$acc.seed" ) or die;
	print A "$_";
	close A;
	push( @accs, $acc );
    }
    close F;
}
else {
    my $db = Rfam::default_db();
    @accs = $db->get_allacc();
}

foreach my $acc ( @accs ) {
    my $file;
    if( $flatfile ) {
	$file = "$acc.seed";
    }
    else {
	$file = "$Rfam::current_dir/$acc/SEED";
    }
    system "$Rfam::scripts_dir/make/aln2ps.pl $file > $acc.ps" and die;
    system "pstopnm --portrait --stdout $acc.ps | pnmtojpeg --quality 90 > $acc.jpg" and die;
    system "pstopnm --portrait --stdout --xsize 200 $acc.ps | pnmtojpeg --quality 90 > tn_$acc.jpg" and die;
    unlink( "$acc.ps" ) or die;
    if( $flatfile ) {
	unlink( "$acc.seed" ) or die;
    }
}

