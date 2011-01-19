#!/software/bin/perl -w

=head1 NAME

rfclnew.pl

=head1 DESCRIPTION
#A script to check a new family into the rcs database & rdb.
#1. QC Checks local files & rdb locks
#2. Parse and validate a CLANDESC file in the given directory.
#3. Assign a unique and stable accession  
#4. Check the file into the rcs system
#5. Load the data into the rfamlive DB.

=head1 AUTHOR

jd7 & pg5 hashup

=cut


BEGIN {
    $rfam_mod_dir = "/software/rfam/scripts/Modules"; 
}

use lib $rfam_mod_dir;
use strict;
use Rfam;
use RfamRCS;
use Getopt::Long;
use Rfam::Clans::Clan;
use Rfam::Clans::Clan_RCS;

my ($id, $help, $skip);

&GetOptions(
	    '-id=s'=> \$id,
	    '-skip' => \$skip,
	    "h|help"         => \$help 
	    );

if( $help ) {
    &help();
    exit(1);
}

if (! $id){
    die "Usage: rfclnew.pl -id <id> -- where <id> is a directory containing a legal CLANDESC file!\n";
}

if( $id =~ /\/$/ ) {
    $id =~ s/\/$//g;
}


#QC checks###########################

#check rcs db not locked & checks on local files

#this is just a simple locked file placed in the ACCESSIONS directory- is for the whole rcs db
my( $locked, $locker, $allow_ref ) = &RfamRCS::check_database_isnot_locked;
if( $locked ) {
    my $allowed;
    foreach my $allow ( @{$allow_ref} ) {
	$allowed = 1 if( $allow eq "rfnew" );
    }
    unless( $allowed ) {
	die "rfnew: Abort - database is locked by $locker";
    }
}

#local files
if (! (-d $id)){
    die "rfclnew: [$id] is not a current directory.\nMust be in the parent directory of the family to make a new family.\n";
}

if (! (-s "./$id/CLANDESC")){
    die "FATAL: rfclnew: clandesc file does not exist or is empty [./$id/CLANDESC]. [$!]"
}

if(   !(-w $id) ) {
    die "rfclnew: [$id] you dont have write permissions to this directory so cant proceed from here\n";
}

#really dont actually want to keep this in here-its a hack due to fail rdb checkin- I blame paul code
if ($skip) {
    my $DBdesc = Rfam::Clans::Clan::slurpClanDesc("./$id/CLANDESC");
    if ( ! ($DBdesc->{'ID'}) || ! $DBdesc->{'AC'}){
	die "Problem with skip- the local desc file does not have an id or acc!!!\n";
    }elsif ($DBdesc->{'ID'} ne $id) {
	die "Problem with skip- the local desc file id does not match the given id\n";
    }else{
	#check it into the RDB
	 eval {
	     Rfam::Clans::Clan::check_into_DB($DBdesc);
	 }; if ($@){
	     print STDERR "Problem with the RDB checkin\n";
	 }else {
	     print STDERR "RDB checkin completed\n\n";
	 }
	exit;
   }
}

# check id is new -checks clan_accmap.dat which is a DB_file hash- this makes a rcs obj...
my $exists_acc;
if ( $exists_acc= &Rfam::Clans::Clan_RCS::id_exists( $id )){
    die "rfclnew: Clan id [$id] already exists in the rcs as clan: [$exists_acc].\nIf this is an existing family, check in the revision using rfclci.pl\n";
}

####################################

print STDERR "\nStarting the RCS check in for $id -- committed now........\n";

#1 Allocate a new stable accession: locks and unlocks the rdb while doing this (simple lock file in ACCESSION dir))
#also updates the clanaccmap.dat file this is a DB_file hash in CLAN_ACCESSION

my $acc=&Rfam::Clans::Clan_RCS::allocate_new_clan_accession( $id );
if( !defined $acc ) {
    die "rfclnew: Unable to allocate new accession number. Check write permission to CLAN_ACCESSION dir\n";
}
print STDERR "1. Assigned new accession and added new clan to accmap\n";  
  
#######################################

# slurp DESC and set ID and ACC in desc file obj
my $desc = Rfam::Clans::Clan::slurpClanDesc("./$id/CLANDESC");
$desc->{'AC'} = $acc;
$desc->{'ID'} = $id;

#####################################

#everything below this is simple RCS

#2 make directory for new clan in rcs master and rcs dir
 if( ! &Rfam::Clans::Clan_RCS::make_new_rcs_directory($acc) ) {
     die "rfclnew: Cannot make a new directory for $acc.\nCheck you have write permissions to RCS_CLAN and CLANS\n";
 }
print STDERR "2. Made new dir in RCS_CLAN AND CLANS for the new clan : [$acc] \n";

#2b lock clan and update local clandesc
my $tag="First lock due to rfnew";
&Rfam::Clans::Clan_RCS::lock_clan($acc, $tag);

open(DE, "> $id/CLANDESC.$$");
Rfam::Clans::Clan::writeClanDesc($desc, \*DE);
close(DE);

if( !rename("$id/CLANDESC.$$","$id/CLANDESC") ) { 
    die "rfclnew: Could not rename temp desc file [$!]";
}

#3 write new CLANDESC file to rcs master directory
 if( &Rfam::Clans::Clan_RCS::move_files_to_rcs_directory($acc, $id) == 0 ) {
     die "rfclnew: Could not move local files to RCS directory for clan [$acc]. Problem!\n";
 }
 print STDERR "3. Coppied local file $id/CLANDESC to RCS_CLAN\n";


####################
#4 make new rcs files in RCS master

my $comment="New clan";

if( &Rfam::Clans::Clan_RCS::make_new_rcs_files($acc,$comment) == 0 ) {
    die "rfclnew: Could not make new CLANDESC for clan [$acc]. Problem!\n";
}

print STDERR "4. Made new rcs files in RCS_CLAN for $acc/CLANDESC\n"; 


##############################
#5 check in rcs files in 

my $date = gmtime();
my $log="[$acc] deposited on $date";

if( &Rfam::Clans::Clan_RCS::check_in_new_rcs_files($acc,$log) == 0 ) {
    die "rfclnew: could not check in CLANDESC for family $acc. Problem!\n";
}

&Rfam::Clans::Clan_RCS::unlock_clan($acc);
print STDERR "5. Done rcs check in of new CLANDESC for $acc and unlocked clan\n"; 


##############################
#6 update current dir;

if( &Rfam::Clans::Clan_RCS::update_current_directory($acc) == 0 ) {
    die "rfclnew: Could not update current directory for $acc\n";
}
print STDERR "6. Updated the current dir with latest ci version of CLANDESC for $acc\n"; 

#######################

print STDERR "Completed: created new rcs entry for $acc\n\n";

######################
#RDB checkin#########
######################

#this is all code in Clan.pm- checkintoDB.
 eval {
   Rfam::Clans::Clan::check_into_DB($desc);
    }; if ($@){
	print STDERR "Problem with the RDB checkin\n";
    }else {
	print STDERR "RDB checkin completed\n\n";
    }

print STDERR "\n\nChecked in clan [$id]to RCS and RDB with accession [$acc]\n";

exit(0);


#################################

sub help{
    print STDERR <<EOF;

Usage:   rfclnew.pl -id <id> -- where <id> is a directory containing a legal CLANDESC file!
        
  Options:   -skip Allows just checkin to the RDB of a new clan. 

To add:

Some sensible RDB error checking  required- current code is a hack of pg5 original. Its not great.

EOF
}
