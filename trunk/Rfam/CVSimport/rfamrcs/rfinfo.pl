#!/usr/local/bin/perl -w

#
# pfinfo written by Ewan Birney, birney@sanger.ac.uk
# ported to rfinfo by sgj
#

use lib '/pfam/db/Rfam/scripts/Modules';

use strict;
use Rfam;
use RfamRCS;
use Getopt::Std;

use vars qw($opt_a
	    $opt_l
	    $opt_v
	    $opt_s
	    $opt_n);


my ($check_family,
    $short_info,
    @families);

if( not &getopts('alvns')) {
    &leave("Unknown arguments for rfinfo");
}

if( !$opt_a && !$opt_l && $#ARGV == -1 ) {
    &usage();
    exit(0);
}

if( $opt_a) {
    @families = &RfamRCS::get_all_family_names();
    $check_family = 0;
    $short_info = 1;
} elsif ( $opt_l ) {
    print STDERR "Scanning RCS directory...this takes some time...";
    @families = &RfamRCS::get_locked_families(\*STDOUT);
    print STDERR "Finished\n";
    if( $#families == -1 ) {
	print STDERR "No locked families\n";
	exit(0)
    }
    $short_info = 1;
} else {
    @families = @ARGV;
}

if( $opt_v ) {
    $short_info = 0;
}

if( $opt_s ) {
    $short_info = 1;
}

#if( $opt_n ) {
#    @families = ();
#    my $db = Bio::Pfam::default_db();
#    foreach my $acc ( @ARGV ) {
#	push(@families,$db->acc2id($acc));
#    }
#}

if( $#families == -1 ) {
    print("rfinfo: We have no families to process... presumably a bug... please contact pfam\@sanger.ac.uk\n");
    exit();
}

foreach my $family (@families) {
    if( $check_family ) {
	if( ! &RfamRCS::check_family_directory_exists($family) ) {
	    print("rfinfo: Family $family does not exist... can't find any info\n");
	    next;
	}
    }

    if( $short_info == 1 ) {
	&RfamRCS::get_short_info_on_family($family,\*STDOUT);
    } else {
	print STDOUT "\nRevision information for $family:\n\n";
	&RfamRCS::get_info_on_family($family,\*STDOUT);
	if( !open(DESC,"$current_dir/$family/DESC") ) {
	    print "rfinfo - could not open $current_dir/$family/DESC - not happy - $!\n";
	} else {
	    print "\nDescription file:\n\n";
	    while( <DESC> ) {
		print;
	    }
	    close DESC;
	}
    }
}



sub leave {
    my $message = shift;

    print "pfinfo exitted due to [$message]\n";
    &usage();
    exit(1);
}

sub usage {
    print STDERR << "EOF";
rfinfo - Information on Rfam families

Usage: rfinfo <options> family-names

Options
 -a All families
 -l locked families
 -n the names are accession numbers
 -v force verbose output (default for normal families)
 -s force short   output (default for -a,-l)
EOF

}





