#!/software/bin/perl -w

# This script compares the working and current versions of the 
# full alignment to check that no sequences are lose.
# This script also checks if any new sequences are found. 
# This script checks whether any sequences in SEED are not
# present in the full alignment.

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/software/rfam/scripts/Modules";
    $bioperl_dir =
        (defined $ENV{'BIOPERL_DIR'})
            ?$ENV{'BIOPERL_DIR'}:"/lustre/pfam/db/bioperl";
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
    if( $acc =~ /\/$/ ) {
	chop $acc;
    }

    my (%current_names,%edited_names,%edited_seed_names);

    # Look at edited version SEED and ALIGN
    my $newali = new Rfam::RfamAlign;
    open (ALI, "$acc/ALIGN")||die "Can't open file $acc/ALIGN\n";
    $newali->read_stockholm(\*ALI);
    close (ALI);
    foreach my $seq ($newali->each_seq() ) {
	my( $acc ) = $seq->id() =~ /^(\S+?)(\.|$)/;
	$edited_names{ $acc } = 1;
    }

    $newali = new Rfam::RfamAlign;
    open (ALI, "$acc/SEED")||die "Can't open file $acc/SEED\n";
    $newali->read_stockholm(\*ALI);
    close (ALI);
    foreach my $seq ($newali->each_seq() ) {
	my( $acc ) = $seq->id() =~ /^(\S+?)(\.|$)/;
	$edited_seed_names{ $acc } = 1;
    }

    # Check all SEED members in ALIGN
    foreach my $element (sort keys %edited_seed_names) {
	if (! $edited_names{$element}) {
	    print STDERR "SERIOUS ERROR: $element in SEED in not in ALIGN!\n";
	}
    }

    if( not $db->is_acc( $acc ) ) {
	print STDERR "NEW FAMILY\n";
	next;
    }

    my $entry = $db->get_Entry_by_acc($acc);
  
    # Look at current version
    my $align=$entry->full();
    my @ids;
    foreach my $seq ($align->each_seq() ) {
	my( $acc ) = $seq->id() =~ /^(\S+?)(\.|$)/;
	$current_names{ $acc } = 1;
    }
    
    # Find missing sequences in edited family
    my %missing;
    my $lost = 0;
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

    my $extra = 0;
    # Find how many sequences gained
    open (FOUND, "> $acc/found")||die "Can't write to file $acc/found\n";
    foreach my $element (sort keys %edited_names){
	if (! $current_names{$element}){
	    print FOUND "$element found\n";
	    $extra++;
	}
    }
    close (FOUND);
	if ($lost == 0 && $extra == 0){
	    print STDERR "No change in SEED and ALIGN members\n";
	}else{
	    print STDERR "Lost $lost. Found $extra. See the missing and found files for details\n";
	}
}


__END__
