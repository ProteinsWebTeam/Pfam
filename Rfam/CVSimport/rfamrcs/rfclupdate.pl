#!/software/bin/perl -w

#provide a local copy of the latest RCS version for a given clan

BEGIN {
    $rfam_mod_dir = "/software/rfam/scripts/Modules";
}


use lib $rfam_mod_dir;
use strict;
use Rfam;
use RfamRCS;
use Getopt::Long;
use Rfam::Clans::Clan_RCS;

my ($acc, $help, $skip);

&GetOptions(
	    '-acc=s'=> \$acc,
            '-skip' => \$skip, 
	    "h|help"         => \$help
	    );

if( $help ) {
    &help();
    exit(1);
}

if (! $acc){
    die "Usage: rfclupdate.pl -acc <CLXXXXX> -- where <CLXXXX> is a valid clan accession\n";
}

if( $acc =~ /\/$/ ) {
    $acc =~ s/\/$//g;
}

#check someone hasnt locked the database
my ($locked, $locker, $allow_ref) = &RfamRCS::check_database_isnot_locked;
OK:{
    if ("$locked") {
	foreach my $allow (@{$allow_ref}) {
	    last OK if( $allow eq "rfupdate" );
	}
    }
    die "rfupdate aborted: database is locked by $locker" if $locked; 
}
		
#check accession and correct files exists in RCS
my $db = Rfam::clan_db();
if( ! ( -d "$Rfam::clan_rcs_dir/$acc")  ) {
    print("clupdate: Clan [$acc] does not have an RCS directory in $Rfam::clan_rcs_dir.\n Unable to find files!\n");
    next; # back to foreach
}

if( ! ( -s "$Rfam::clan_rcs_dir/$acc/CLANDESC,v") ){
    print STDERR "\n\nAlthough there is the directory [$acc], the CLANDESC files is not there\n; This suggests a major problem...\n\n";
}

#check if anyone currently has the lock on the clan
my ($islocked,$lockedpeople) = &Rfam::Clans::Clan_RCS::check_clan_is_not_locked($acc);

if( $islocked ) {
    print("rfupdate: Just to warn you; family [$acc] has been locked by [$lockedpeople]\n");
}

#make a local directory and copy from clan current dir.
if( !(-d $acc) ) {
    mkdir "$acc", 0777  || die "Could not make directory $acc, error message $!";
}

chdir $acc || die "Could not change directory in $acc, error message $!";

if( system("cp $Rfam::clan_dir/$acc/CLANDESC ./") != 0 ) {
    print STDERR "problem with copy of CLANDESC from the current\n";
}

if( system("chmod u+w ./CLANDESC") != 0 ) {
    print STDERR "prbolem with setting permissions on the clandesc file\n";
}

print STDOUT "\n\nUpdated clan [$acc]\n";

chdir ".." || die "Could not ascend one directory. Whoops! $!";    


exit(0);
#################################

sub help{
    print STDERR <<EOF;

Usage:   rfclupdate..pl -acc <CLXXXXX> -- where <acc> is a valid accession for a clan
        

EOF
}
