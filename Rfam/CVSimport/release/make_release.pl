#!/usr/local/bin/perl -w

use strict;
use lib '/pfam/db/Rfam/scripts/Modules';
use Rfam;
use RfamRCS;

my $rel = shift;
$rel =~ s/_/\./;
my $error;

if( not -d "$Rfam::releases_dir/$rel" ) {
    mkdir( "$Rfam::releases_dir/$rel", 0755 );
}

my @family_list = &Rfam::get_allaccs();

print STDERR "Checking view files ....\n";
foreach my $acc ( @family_list ) {
    my $id = Rfam::acc2id( $acc );
    if( &RfamRCS::view_file_errors( $id ) ) {
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
