#!/usr/local/bin/perl -w

#
# author sgj
# borrowed heavily from Ewan's pfam code
#

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
}

use lib $rfam_mod_dir;

use strict;
use Getopt::Std;
use Rfam;
use RfamRCS;

use vars qw ($opt_u);
&getopt("u:");
my $name = $opt_u;

if( $#ARGV == -1 ) {
    &RfamRCS::show_rcs_help(\*STDOUT);
    exit;
}

my ($locked, $locker, $allow_ref) = &RfamRCS::check_database_isnot_locked;
if( $locked ) {
    my $allowed;
    foreach my $allow ( @{$allow_ref} ) {
        $allowed = 1 if( $allow eq "pfabort" );
    }
    unless( $allowed ) {
        die "pfabort: Aborted - database is locked by $locker";
    } 
}

my $fam = shift;
my $acc;
my $db = Rfam::default_db();
if( $db->is_id($fam) ) {
    $acc = $db->id2acc( $fam );
    die "rfabort: Cannot find accession for $fam\n" if not $acc;
}
else {
    $acc = $fam;
}

if( ! &RfamRCS::check_family_directory_exists($acc) ) {
    die("rfabort: Family [$acc] does not have a directory in RCS_MASTER.\nYou should probably mail pfam\@sanger.ac.uk if you think this is a real family");
}

if( ! &RfamRCS::check_family_exists($acc) ) {
    die("rfabort: Family [$acc] has RCS_MASTER directory, but not the correct files internally.\nPlease contact pfam\@sanger.ac.uk to resolve the error");
}
    
if (not defined($name)) {
    $name = `whoami`;
    chop $name;
}

my ($haslocked,$peoplelocked) = &RfamRCS::user_has_locked_family($acc,$name);

if( $haslocked == 0 ) {
    die ("rfabort: This family [$acc] was not locked by you [$name],\nit was locked by [$peoplelocked]... so can't abort\n");
}

if( ! &RfamRCS::abort_lock_on_family($acc) ) {
    die("rfabort: Unable to abort lock ... this is a bad internal error\nPlease report problem to pfam\@sanger.ac.uk");
}
    
print STDOUT "\n\nAborted lock on family [$acc]\n";











