#!/usr/local/bin/perl

# This script compares the working and current versions of the 
# full alignment to check that no sequences are lose.
# This script also checks if any new sequences are found. 
# This script checks whether any sequences in SEED are not
# present in the full alignment.

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
    $bioperl_dir =
        (defined $ENV{'BIOPERL_DIR'})
            ?$ENV{'BIOPERL_DIR'}:"/pfam/db/bioperl";
}

use lib $rfam_mod_dir;
use lib $bioperl_dir;

use strict;
use Rfam;
use Rfam::RfamAlign;
use Getopt::Long;

my $db = Rfam::default_db();

if( $#ARGV == -1 ) {
    print "rqc-check.pl. Checks no members of family have been lost.\nUsage rqc-check.pl <family>\n";
    exit(1);
}

foreach my $acc (@ARGV) {
    my $entry = $db->get_Entry_by_acc($acc);
    my ($ali,$nse,$start,$end,$extra);
    my (%current_names,%edited_names,%edited_seed_names);
  
    # Look at current version
    my $align=$entry->full();
    my @ids;
    foreach my $seq ($align->eachSeq() ) {
	$current_names{ $seq->id() } = 1;
    }
    
    # Look at edited version SEED and ALIGN
    my $newali = new Rfam::RfamAlign;
    open (ALI, "$acc/ALIGN")||die "Can't open file $acc/ALIGN\n";
    $newali->read_stockholm(\*ALI);
    close (ALI);
    foreach my $seq ($newali->eachSeq() ) {
	$edited_names{ $seq->id() } = 1;
    }

    $newali = new Rfam::RfamAlign;
    open (ALI, "$acc/SEED")||die "Can't open file $acc/SEED\n";
    $newali->read_stockholm(\*ALI);
    close (ALI);
    foreach my $seq ($newali->eachSeq() ) {
	$edited_seed_names{ $seq->id() } = 1;
    }

    # Find missing sequences in edited family
    my (%missing,$lost);
    # Put missing sequences into a missing file in directory
    open (MISSING, "> $acc/missing")||die "Can't write to file $acc/missing\n";
    foreach my $element (sort keys %current_names){
	if (! $edited_names{$element}){
	    print MISSING "$element not found\n";
	    # Add element to missing hash
	    $missing{$element}=1;
	    $lost++;
	}
    }
    close (MISSING); 

    # Find how many sequences gained
    open (FOUND, "> $acc/found")||die "Can't write to file $acc/found\n";
    foreach my $element (sort keys %edited_names){
	if (! $current_names{$element}){
	    print FOUND "$element found\n";
	    $extra++;
	}
    }
    close (FOUND);
    print "Lost $lost. Found $extra.\n";

    # Check all SEED members in ALIGN
    foreach my $element (sort keys %edited_seed_names) {
	if (! $edited_names{$element}) {
	    print "SERIOUS ERROR: $element in SEED in not in ALIGN!\n";
	}
    }
}


__END__
