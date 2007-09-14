#!/software/bin/perl -w

# jd7 fixed for lustre move

# Author sgj
# heavily borrowed from Ewan's pfam code


BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
}

use lib $rfam_mod_dir;

use strict;
use Getopt::Long;
use Rfam;
use RfamRCS;

my( $help );
&GetOptions( "h"    => \$help );

if( $help ) {
    &RfamRCS::show_rcs_help(\*STDOUT);
    exit;
}

## first, check that the database is not locked

my ($locked, $locker, $allow_ref) = &RfamRCS::check_database_isnot_locked;
OK:{
    if ("$locked") {
	foreach my $allow (@{$allow_ref}) {
	    last OK if( $allow eq "rfco" );
	}
    }
    die "rfco aborted: database is locked by $locker" if $locked; 
}

my $fam = shift;
my $acc;
my $db = Rfam::default_db();
if( $db->is_id($fam) ) {
    $acc = $db->id2acc( $fam );
    die "rfco: Cannot find accession for $fam\n" if not $acc;
}
else {
    $acc = $fam;
}

if( ! ( -d "$Rfam::rcs_master_dir/$acc")  ) {
    die "rfco: Family [$acc] does not have an RCS directory in $Rfam::rcs_master_dir.\nUnable to check out\n";
}

if( &RfamRCS::check_family_exists($acc) == 0 ) {
    die "rfco: Although there is the directory [$acc], not all the correct files are there\nThis suggests a major problem...\nPlease get in contact with pfam\@sanger.ac.uk\n";
}

my ($islocked,$lockedpeople) = &RfamRCS::check_family_isnot_locked($acc);

if( $islocked ) {
    die "rfco: Family [$acc] has been locked by [$lockedpeople]\ncannot check out\n";
}

if( !(-d $acc) ) {
    mkdir "$acc", 0777  or die "Could not make directory $acc, error message $!";
}
chdir $acc or die "Could not change directory in $acc, error message $!";

if( !(&RfamRCS::check_out_family($acc,\*STDOUT)) ) {
    die "rfco: Very bad error on family $acc, cannot check out\nPlease email pfam\@sanger.ac.uk to resolve this problem!\n";
}
  
if( !(&RfamRCS::move_family_to_current($acc)) ) {
    die "rfco: Could not move family. Not happy about this! $!\n";
}

print STDOUT "\n\nChecked out family [$acc]\n";



