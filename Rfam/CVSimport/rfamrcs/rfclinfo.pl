#!/software/bin/perl -w


BEGIN {
    $rfam_mod_dir = "/software/rfam/scripts/Modules";
}


use lib $rfam_mod_dir; 

use strict;
use Rfam;
use RfamRCS;
use Rfam::Clans::Clan_RCS;
use Getopt::Std;

use vars qw($opt_a
	    $opt_l
	    $opt_v
	    $opt_s
	    $opt_n);

my ($check_family,
    $short_info,
    @clans);

my $db = Rfam::clan_db(); #clan rcs

if( not &getopts('alvns')) {
    &leave("Unknown arguments for rfclinfo");
}

if( !$opt_a && !$opt_l && $#ARGV == -1 ) {
    &usage();
    exit(0);
}

#check options
if( $opt_a) {
    @clans = $db->get_allacc();
    $check_family = 0;
    $short_info = 1;
} elsif ( $opt_l ) {
    print STDERR "Scanning RCS directory...this takes some time...";
    @clans = &Rfam::Clans::Clan_RCS::get_locked_families(\*STDOUT);
    print STDERR "Finished\n";
    if( $#clans == -1 ) {
	print STDERR "No locked families\n";
	exit(0)
    }
    $short_info = 1;
} else {
    @clans = @ARGV;
}

if( $opt_v ) {
    $short_info = 0;
}

if( $opt_s ) {
    $short_info = 1;
}

if( $#clans == -1 ) {
    print("rfinfo: We have no clans to process... presumably a bug...\n");
    exit();
}

foreach my $acc (@clans) {

    if( $check_family ) {
	#check is an exsisting rcs dir
	if( ! ( -d "$Rfam::clan_rcs_dir/$acc")  ) {
	    print STDERR "rfclco: Clan [$acc] does not have an RCS directory in $Rfam::clan_rcs_dir.\nNo info available\n";
	    next;
	}

        #check the desc file is there
	if( ! ( -e "$Rfam::clan_rcs_dir/$acc/CLANDESC,v")  ) {
	    print STDERR "rfclinfo: Clan [$acc] has rcs dir but not have relevant RCS file in $Rfam::clan_rcs_dir/$acc  this isnt good\n";
	    next;
	}
    }

    if( $short_info ) {
	&Rfam::Clans::Clan_RCS::get_short_info_on_clan($acc,\*STDOUT);
    } else {
	print STDOUT "\nRevision information for $acc:\n\n";
	&Rfam::Clans::Clan_RCS::get_info_on_clan($acc,\*STDOUT);
	if( !open(DESC,"$Rfam::clan_dir/$acc/CLANDESC") ) {
	    print "rfclinfo - could not open $Rfam::clan_dir/$acc/DESC - not happy - $!\n";
	} else {
	    print "\nDescription file:\n\n";
	    while( <DESC> ) {
		print;
	    }
	    close DESC;
	}
    }
}







##########################################################

sub leave {
    my $message = shift;

    print "rfclinfo exitted due to [$message]\n";
    &usage();
    exit(1);
}

sub usage {
    print STDERR << "EOF";
rfclinfo - Information on Rfam Clans

Usage: rfclinfo <options> family-names

Options
 -a All families
 -l locked families
 -n the names are accession numbers
 -v force verbose output (default for normal families)
 -s force short   output (default for -a,-l)
EOF

}
