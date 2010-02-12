#!/software/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/software/rfam/scripts/Modules";
    $bioperl_dir = 
        (defined $ENV{'BIOPERL_DIR'})
            ?$ENV{'BIOPERL_DIR'}:"lustre/pfam/db/bioperl";
}

use lib $bioperl_dir;
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
    my $psfile;
    if ($acc eq "RF00017" && -s "/lustre/pfam/rfam/Production/Rfam/PICTURES/$acc.jpg" ){
	$psfile = "/lustre/pfam/rfam/Production/Rfam/PICTURES/$acc.jpg";
	system ("convert $psfile -resize 800x800 $acc.jpg") and die;
	system ("convert $psfile -resize 300x300 tn_$acc.jpg") and die;
	next;
    }
    if( -s "/lustre/pfam/rfam/Production/Rfam/PICTURES/$acc.ps" ) {
	$psfile = "/lustre/pfam/rfam/Production/Rfam/PICTURES/$acc.ps";
	system ("convert $psfile -quality 95 -density 144 -rotate 90 $acc.jpg") and die;
        system ("convert $psfile -resize 195x195 -quality 95 -density 144 -rotate 90 tn_$acc.jpg")and die;
    }
    else {
	$psfile = "./$acc.ps";
	my $file;
	if( $flatfile ) {
	    $file = "$acc.seed";
	}
	else {
	    $file = "$Rfam::current_dir/$acc/SEED";
	}
	system "$Rfam::scripts_dir/make/aln2ps.pl $file > $psfile" and die;
	system ("convert $psfile -quality 95 -density 144 $acc.jpg") and die;
        system ("convert $psfile -resize 195x195 -quality 95 -density 144 tn_$acc.jpg")and die;
     }
    if( -s "./$acc.ps" ) {
	unlink( "./$acc.ps" ) or die;
    }
    if( $flatfile ) {
	unlink( "$acc.seed" ) or die;
    }

}
unlink( "RF00017.seed" ) or die;

