#!/usr/local/bin/perl


#
# author sgj
# heavily lifted from Ewan's pfam code
#


use lib '/pfam/db/Rfam/scripts/Modules';

use strict;
use Rfam;
use RfamRCS;


if( $#ARGV == -1 ) {
    &RfamRCS::show_rcs_help(\*STDOUT);
    exit;
}

my ($locked, $locker, $allow_ref) = &RfamRCS::check_database_isnot_locked;
OK:{
    if ("$locked") {
	foreach my $allow (@{$allow_ref}) {
	    last OK if( $allow eq "rfupdate" );
	}
    }
    die "rfupdate aborted: database is locked by $locker" if $locked; 
}
		
foreach my $acc (@ARGV) {
    if( ! ( -d "$rcs_master_dir/$acc")  ) {
	print("rfupdate: Family [$acc] does not have an RCS directory in $rcs_master_dir.\n Unable to find files!\n");
	next; # back to foreach
    }

    if( &RfamRCS::check_family_exists($acc) == 0 ) {
	print STDERR "\n\nAlthough there is the directory [$acc], not all the correct files are there\nThis suggests a major problem...\nPlease get in contact with pfam\@sanger.ac.uk\n";
	next;
    }

    my ($islocked,$lockedpeople) = &RfamRCS::check_family_isnot_locked($acc);

    if( $islocked ) {
	print("rfupdate: Just to warn you; family [$acc] has been locked by [$lockedpeople]\n");
    }

    if( !(-d $acc) ) {
	mkdir "$acc", 0777  || die "Could not make directory $acc, error message $!";
    }

    chdir $acc || die "Could not change directory in $acc, error message $!";

    if( &RfamRCS::copy_from_current($acc) == 0 ) {
	print ("rfupdate: Problem - we could not copy the files from the current set.");
    }

    print STDOUT "\n\nUpdated family [$acc]\n";

    chdir ".." || die "Could not ascend one directory. Whoops! $!";    
}

