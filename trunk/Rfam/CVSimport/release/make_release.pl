#!/usr/local/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
}

use lib $rfam_mod_dir;

use strict;
use Rfam;
use RfamRCS;

my $rel = shift;
$rel =~ s/_/\./;
my $error;

if( not -d "$Rfam::releases_dir/$rel" ) {
    mkdir( "$Rfam::releases_dir/$rel", 0755 );
}

my $db = Rfam::default_db();
my @family_list = $db->get_allacc();

print STDERR "Checking view files ....\n";
foreach my $acc ( @family_list ) {
#    my $id = Rfam::acc2id( $acc );
    if( &RfamRCS::view_file_errors( $acc ) ) {
        warn "$acc: found errors with viewfiles\n";
	$error ++;
    }
}

die "View files contain errors, exiting\n" if $error;

unless( -s "$Rfam::releases_dir/$rel/Rfam.seed" ) {
    print STDERR "Making Rfam.seed ....\n";
    if( &RfamRCS::make_align_release_file( "SEED", "$Rfam::releases_dir/$rel" ) ) {
	die "Failed to make Rfam.seed";
    }
}

unless( -s "$Rfam::releases_dir/$rel/Rfam.full" ) {
    print STDERR "Making Rfam.full ....\n";
    if( &RfamRCS::make_align_release_file( "FULL", "$Rfam::releases_dir/$rel" ) ) {
	die "Failed to make Rfam.full";
    }
}

unless( -s "$Rfam::releases_dir/$rel/Rfam.fasta" ) {
    print STDERR "Making Rfam.fasta ....\n";
    system "$Rfam::scripts_dir/release/make_fastadb.pl -i 90 $Rfam::releases_dir/$rel/Rfam.full > $Rfam::releases_dir/$rel/Rfam.fasta" and die;
}

unless( -s "$Rfam::releases_dir/$rel/Rfam.thr" ) {
    print STDERR "Making Rfam.thr ....\n";
    system "$Rfam::scripts_dir/release/make_thr.pl $Rfam::releases_dir/$rel/Rfam.seed > $Rfam::releases_dir/$rel/Rfam.thr" and die;
}

unless( -s "$Rfam::releases_dir/$rel/Rfam.tar" ) {
    print STDERR "Making Rfam.tar ....\n";
    system "$Rfam::scripts_dir/release/make_models.pl > $Rfam::releases_dir/$rel/Rfam.tar" and die;
}

