#!/usr/local/bin/perl -w

use strict;
use Rfam;
use RfamQC;
use RfamRCS;

my $rel = shift;
$rel =~ s/_/\./;
my $error;

print STDERR "--------------------\nMaking release $rel\n--------------------\n";
if( not -d "$Rfam::releases_dir/$rel" ) {
    mkdir( "$Rfam::releases_dir/$rel", 0755 ) or die;
}
chdir "$Rfam::releases_dir/$rel" or die;

my $db = Rfam::default_db();
my @family_list = $db->get_allacc();

unless( -e "checked_sequences" ) {
    print STDERR "Checking sequences ....\n";
    chdir "$Rfam::current_dir" or die "failed to cd to $Rfam::current_dir";
    foreach my $acc ( @family_list ) {
	unless( &RfamQC::valid_sequences( $acc ) ) {
	    warn "$acc: sequence up-to-date check failed\n";
	    $error ++;
	}
    }
    die "Sequences are not up-to-date, exiting\n" if $error;
    chdir "$Rfam::releases_dir/$rel" or die;
}

# check format in here somewhere!

unless( -e "rcs_labelling_done" ) {
    print STDERR "RCS labelling families ....\n";
    my( $major, $minor ) = $rel =~ /(\d+)\.(\d+)/;
    my $label = "Rel$major"."_$minor";
    foreach my $acc ( @family_list ) {
	eval {
	    &RfamRCS::label_family( $acc, $label );
	};
	if( $@ ) {
	    $error ++;
	    warn "$acc: failed to add RCS label [$label]\n";
	}
    }
    die "RCS labelling step failed, exiting\n" if $error;
    system "touch rcs_labelling_done" and die;
}

print STDERR "Checking view files ....\n";
foreach my $acc ( @family_list ) {
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

unless( -s "$Rfam::releases_dir/$rel/Rfam.pics.tar" ) {
    print STDERR "Making Rfam.pics.tar ....\n";
    mkdir( "$Rfam::releases_dir/$rel/pics", 0755 ) or die;
    chdir "$Rfam::releases_dir/$rel/pics" or die;
    system "$Rfam::scripts_dir/release/make_ss_pics.pl $Rfam::releases_dir/$rel/Rfam.seed" and die;
    system "tar -cvf $Rfam::releases_dir/$rel/Rfam.pics.tar RF*.jpg tn_RF*.jpg" and die;
    chdir "$Rfam::releases_dir/$rel/" and die;
    system "rm -rf $Rfam::releases_dir/$rel/pics" and die;
}

