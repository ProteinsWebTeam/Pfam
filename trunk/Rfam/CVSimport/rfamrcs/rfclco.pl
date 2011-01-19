#!/software/bin/perl -w

BEGIN {
    $rfam_mod_dir = "/software/rfam/scripts/Modules";
}

use lib $rfam_mod_dir;
use strict;
use Rfam;
use Getopt::Long;
use RfamRCS;
use Rfam::Clans::Clan;
use Rfam::Clans::Clan_RCS;


my ($acc, $help);

&GetOptions(
	    '-acc=s'=> \$acc,
	    "h|help"         => \$help 
	    );

if( $help ) {
    &help();
    exit(1);
}

if (! $acc){
    die "Usage: rfclco.pl -acc <CLXXXXX> -- where <CLXXXXX> is the name of a clan accession\n";
}

if( $acc =~ /\/$/ ) {
    $acc =~ s/\/$//g;
}

#add method:check format of id
#add method: convert id to acc?

#1 check rcs db not locked
my ($locked, $locker, $allow_ref) = &RfamRCS::check_database_isnot_locked;
OK:{
    if ("$locked") {
	foreach my $allow (@{$allow_ref}) {
	    last OK if( $allow eq "rfco" );
	}
    }
    die "rfco aborted: database is locked by $locker" if $locked; 
}


#check is an rcs dir
if( ! ( -d "$Rfam::clan_rcs_dir/$acc")  ) {
    die "rfclco: Clan [$acc] does not have an RCS directory in $Rfam::clan_rcs_dir.\nUnable to check out\n";
}

#check the desc file is there
if( ! ( -e "$Rfam::clan_rcs_dir/$acc/CLANDESC,v")  ) {
    die "rfclco: Clan [$acc] has rcs dir but not have relevant RCS file in $Rfam::clan_rcs_dir/$acc  this isnt good\n";
}

#check clan isnt locked
my ($islocked,$lockedpeople) = &Rfam::Clans::Clan_RCS::check_clan_is_not_locked($acc);

if( $islocked ) {
    die "rfclco: Clan [$acc] has been locked by [$lockedpeople]\ncannot check out\n";
}else{
    print STDERR "Clan $acc now locked by you\n";
}

#make a local directory #mv into this dir
if( !(-d $acc) ) {
    mkdir "$acc", 0777  or die "Could not make directory $acc, error message $!";
}

chdir $acc or die "Could not change directory in $acc, error message $!";

#check out the family-this is the only bit that does anything scary...
if( !(&Rfam::Clans::Clan_RCS::check_out_clandesc($acc,\*STDOUT)) ) {
    die "rfclco: Very bad error on clan $acc, cannot check out\nPlease email pfam\@sanger.ac.uk to resolve this problem!\n";
}

if( !(&Rfam::Clans::Clan_RCS::move_clandesc_to_local($acc)) ) {
    die "rfco: Could not move family. Not happy about this! $!\n";
}

print STDERR "Clan $acc now checked out to current dir\n";
