#!/software/bin/perl -w


#
# author sgj
# heavily lifted from Ewan's pfam code
#

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
}

use lib $rfam_mod_dir;

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
		
my $db = Rfam::default_db();
foreach my $fam (@ARGV) {
    my $acc;
    if( $db->is_id($fam) ) {
	$acc = $db->id2acc( $fam );
	die "rfupdate: Cannot find accession for $fam\n" if not $acc;
    }
    else {
	$acc = $fam;
    }

    if( ! ( -d "$Rfam::rcs_master_dir/$acc")  ) {
	print("rfupdate: Family [$acc] does not have an RCS directory in $$Rfam::rcs_master_dir.\n Unable to find files!\n");
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

#to fix the annoyance with timestamps at checkout.
my @filelist = qw( DESC SEED CM OUTPUT  out.list TABFILE ALIGN scores scores.eva
lue  species );

foreach my $file (@filelist){
    system("touch $file") if $file;          
}

 print STDOUT "\n\nUpdated family [$acc]\n";

    chdir ".." || die "Could not ascend one directory. Whoops! $!";    
}

