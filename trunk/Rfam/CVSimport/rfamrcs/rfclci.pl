#!/software/bin/perl -w

=head1 NAME

rfclci.pl

=head1 DESCRIPTION

#A script to check an edited family back into the rcs database & rdb.
#1. QC Checks local files & rdb locks
#2. Parse and validate a CLANDESC file in the given directory. 
#3. Check the file into the rcs system
#4. Load the data into the rfamlive DB.
=head1 AUTHOR

jd7@sanger.ac.uk

=cut


BEGIN {
    $rfam_mod_dir = "/software/rfam/scripts/Modules";
}

use lib $rfam_mod_dir; #temporary
use strict;
use Rfam;
use RfamRCS;
use Getopt::Long;
use Rfam::Clans::Clan;
use Rfam::Clans::Clan_RCS;
use Cwd;

my ($acc, $help, $message, $skip);

&GetOptions(
	    '-acc=s'=> \$acc,
	    '-m:s' =>  \$message, 
            '-skip' => \$skip, 
	    "h|help"         => \$help
	    );

if( $help ) {
    &help();
    exit(1);
}

if (! $acc){
    die "Usage: rfclci.pl -acc <acc> -- where <acc> is a directory containing an updated CLANDESC file for ci!\n";
}

if( $acc =~ /\/$/ ) {
    $acc =~ s/\/$//g;
}

my $user=`whoami`;
chomp $user;

#whole database is only locked when accmap is being updated (rfnew.pl/rfclnew.pl)
my ($locked, $locker, $allow_ref) = &RfamRCS::check_database_isnot_locked;
OK:{
    if ("$locked") {
	foreach my $allow (@{$allow_ref}) {
	    last OK if( $allow eq "rfco" );
	}
    }
    die "rfco aborted: database is locked by $locker" if $locked; 
}

#check the local files
if (! (-d $acc)){
    die "rfclnew: [$acc] is not a current directory.\nMust be in the parent directory of the family to make a new family.\n";
}

if (! (-s "./$acc/CLANDESC")){    die "FATAL: rfclnew: clandesc file does not exist or is empty [./$acc/CLANDESC]. [$!]"
}

if(   !(-w $acc) ) {
    die "rfclnew: [$acc] you dont have write permissions to this directory so cant proceed from here\n";
}



#check RCS status#######################

#check is an exsisting rcs dir
if( ! ( -d "$Rfam::clan_rcs_dir/$acc")  ) {
    die "rfclco: Clan [$acc] does not have an RCS directory in $Rfam::clan_rcs_dir.\nUnable to check out\n";
}

#check the desc file is there
if( ! ( -e "$Rfam::clan_rcs_dir/$acc/CLANDESC,v")  ) {
    die "rfclco: Clan [$acc] has rcs dir but not have relevant RCS file in $Rfam::clan_rcs_dir/$acc  this isnt good\n";
}

#dont really want to have this but was needed to update failed RDB checkins- again dodgy pg5 code :-)
if ($skip){
    goto SLURP;
}

#check user has family locked-this lock was added at rfclco.pl
my ($haslocked,$peoplelocked) = &Rfam::Clans::Clan_RCS::user_has_locked_clan($acc,$user);
if( $haslocked == 0 ) {
    die "rfci: This clan [$acc] was not locked by you[$user], it was locked by [$peoplelocked]\n";
}

SLURP:
#read in desc file and check it...
my $CLANDESC="./$acc/CLANDESC";
my $desc;
if (-s "$CLANDESC"){
    $desc = &Rfam::Clans::Clan::slurpClanDesc("$CLANDESC");
}else{
    die "rfclci: A bad error - cannot open the clandesc file [./$acc/CLANDESC]. Yikes! [$!]";
}

####got to put some checks in here on new desc file
if(!($desc->{'AC'}) || ($desc->{'AC'} ne $acc) ) {
    die "rfci: Your DESC file has no AC line or its different than specified -- This is bad!\n";
}

#check current id is the same as in the accmap:
my $id= $desc->{'ID'};
my $exists_acc;
if (  (&Rfam::Clans::Clan_RCS::id_exists($id)) ne ($desc->{'AC'}) ) {
    die "This id is not already in the accmap for this acc -it has changed-deal with this-this means adding methods to update the accmap and so on which I kindly havent added yet.\n";
}else{
    print STDERR "ID fine so go on with ci\n\n";
}

if ($skip){
    eval {
   Rfam::Clans::Clan::check_into_DB($desc);
    }; if ($@){
	print STDERR "Problem with the RDB checkin\n";
    }else {
	print STDERR "RDB checkin completed\n§n";
    }
    
   exit;
}

#below this does stuff so careful.

#edits to the desc file
my $dir=getcwd();

if( &Rfam::Clans::Clan_RCS::move_files_to_rcs_directory($acc, $acc) == 0 ) {
    die "rfclnew: Could not move local files to RCS directory for clan [$acc]. Problem!\n";
}


#get message if message not provided
my $comment;
if( !defined $message ) {
    print "Please give a comment for clan [$acc]\n";
    print "Finish comment by a . on the line by itself\n"; 

    while( <STDIN> ) {
	chop;
	/^\s*\.\s*$/ && last;
	$comment .= "$_\n";
    }
} else {
    $comment = $message;
}

#check in rcs files and remove family lock (lock placed at checkout)
if( &Rfam::Clans::Clan_RCS::check_in_new_rcs_files($acc,$comment) == 0 ) {
    die "rfclnew: could not check in CLANDESC for family $acc. Problem!\n";
}else{
    &Rfam::Clans::Clan_RCS::unlock_clan($acc);
}

#update current dir
if( &Rfam::Clans::Clan_RCS::update_current_directory($acc) == 0 ) {
    die "rfclnew: Could not update current directory for $acc\n";
}

print STDERR "Finished RCS checkin for clan [$acc]\n"; 

#load rdb:
 eval {
   Rfam::Clans::Clan::check_into_DB($desc);
    }; if ($@){
	print STDERR "Problem with the RDB checkin\n";
    }else {
	print STDERR "Finished RDB checkin\n\n";
    }

print STDERR "\n\nClan [$acc], [$id] checked into RCS and RDB\n";

exit(0);

#################################

sub help{
    print STDERR <<EOF;

Usage:   rfclci.pl -acc <CLXXXXX> -- where <CLXXXXXX> is a directory containing a legal CLANDESC file!
        
  Options:   -skip Allows just checkin to the RDB of a new clan. 

To add:

Some sensible RDB error checking  required- current code is a hack of pg5 original. Its not great.

EOF
}
