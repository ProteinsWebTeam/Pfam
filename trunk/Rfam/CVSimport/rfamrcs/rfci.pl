#!/software/bin/perl -w

=head1 NAME

rfci.pl

=head1 DESCRIPTION

New rfci.pl code edited from the old rfci.pl code by jd7 -uses existing Modules-with edits...

-place lock flag on RDB--dont want the rdb to be changed while RCS checkin

RCS checkin:
-pretty much unchanged from before except used the Rfam::Utils to slurp the DESC file and fix it.

RDB checkin:
Only begins when the RCS checkin is complete-majority of method calls are in UpdateRDB.pm
Uses an Entry obj (EntryRCS) to get the data for loading to RDB

-remove lock flag  on rdb


=head1 AUTHOR

jd7 (edits on the old rfci.pl code)

=cut


BEGIN {
   $rfam_mod_dir = "/software/rfam/scripts/Modules/";
}

use lib $rfam_mod_dir;
use IO::File;
use strict;
use Rfam;
use RfamRCS;
use RfamQC;
use Getopt::Long;
use DBD::mysql;
use vars qw($opt_a);
use DateTime;
use RfamUtils;
use Cwd;
use File::Copy;

my ($acc, 
    $remove,
    $message,
    $updateRDB,
    $help);

&GetOptions(
	    '-acc=s'=> \$acc,
	    '-remove:s' => \$remove,
	    '-m:s' => \$message,
	    '-updateRDB' => \$updateRDB,
	    "h|help"         => \$help 
	    );

if( $help ) {
    &help();
    exit(1);
}

#get the user
my $user =  getlogin() || getpwuid($<);

#start checkin time:
my $start_time=time();
my $rcs_time;

#RCS database
my $db = Rfam::default_db();

#Rfamlive database
my $rdb = Rfam::live_rdb_update(); # curation new UpdateRDB obj
my $rdb_admin=Rfam::live_rdb_update_admin();

if (!$acc){
    print STDERR "Family accession must be provided using -acc\n";
}

if( !(-d $acc) ) {
    die "rfci: [$acc] is not a current directory.\nMust be in the parent directory of the family to check in\n";
}
#need to add in this for checking the qc has been done-dont allow ci otherwise.
#    if (! -e "$acc/qc.passed") ){
#    die "rfci: [$acc] has not been passed by qc checks so not ready to check in - run rqc-all.pl\n";
#    }

####

if ($updateRDB){ 
    $rcs_time=time(); 
    goto LOCK;
}


#check RCS lock###################################

my ($rcs_locked, $rcs_locker, $allow_ref) = &RfamRCS::check_database_isnot_locked;
if( $rcs_locked ) {
   my $allowed;
   foreach my $allow ( @{$allow_ref} ) {
	$allowed = 1 if( $allow eq "rfci" );
   }
   unless( $allowed ) {
	die "rfci aborted: database is locked by $rcs_locker";
   } 
}

#check given acc#####################################

if( ! &RfamRCS::check_family_directory_exists($acc) ) {
    die "rfci: Family [$acc] does not have a directory in RCS_MASTER.\nIf this is a new family, check in a new family with rfnew\n";
}

if( ! &RfamRCS::check_family_exists($acc) ) {
    die "rfci: Bad error Family [$acc] has RCS_MASTER directory, but not the correct files internally.\n";
}

#check family not locked already
my ($haslocked,$peoplelocked) = &RfamRCS::user_has_locked_family($acc,$user);
if( $haslocked == 0 ) {
    die "rfci: This family [$acc] was not locked by you[$user], it was locked by [$peoplelocked]\n";
}

#check the DESC 
my $dir=cwd;
my $DESC="$dir/$acc/DESC";
my $desc;
if (-s "$DESC"){
    $desc = RfamUtils::slurpDesc("$DESC");
}else{
    die "rfci: A bad error - cannot open the desc file [./$acc/DESC]. Yikes! [$!]";
}

if(! $desc->{'AC'}) {
    die "rfci: Your DESC file has no AC line. This is bad!\n";
}

########################
#All is apparently ok-proceed with a checkin

# #Lock RDB
# #this opens a connection, adds lock to RDB table and then closes it
LOCK:
my $lock;
$lock=$rdb->add_lock($user, $acc);

if ($lock){
    if  ($lock->{'status'}){
	print STDERR "Successful lock on the RDB to allow checkin by $user\n";
    }elsif($lock->{'locker'} eq $user && $lock->{'family'} ne $acc){
	die  "You already have the lock on the RDB for a different family", $lock->{'locker'},",", $lock->{'family'},"\n";
    }
    else {
	#lock by someone else
	die "RDB currently has lock placed on it by ". $lock->{'locker'},",", $lock->{'family'}, "-you cant check in right now\n";
    }
}
else{
	die  "Problem obtaining lock on RDB for $user\n";
}

if ($updateRDB) {goto RDB;}
############################

# #check ID line not changed if so fix DES file;
my $oldid=$db->acc2id($acc);

unless ($desc->{'ID'} eq $oldid){
 
   print STDERR "ID line has changed\n";
    #check the new id-if its not valid-die and break the RDB lock
    if (my $acccheck= &RfamQC::id_exists($desc->{'ID'})){
	if ($acccheck ne $desc->{'AC'}){
	    $lock=$rdb->remove_my_lock($user, $lock); 
	    die "rfci: your new id already exists[$acccheck]-cant check in until you fix this\n";
	}
    }

    #get the RCS lock-if not die and break the RDB lock
     if( my $ret = $db->_get_lock() ) {
	$lock=$rdb->remove_my_lock($user, $lock);  
	die "rfci: The accession lock has been grabbed by [$ret].\n".
            "Accession locking should be short - try again in a couple of minutes\n";
    }
    #otherwise RDB and RCS family lock is on:
    #check the PI line hasnt already been updated by the submitter
    my $currentPI=$desc->{'PI'};
    if ($currentPI!~/^$oldid/){

	#rewrite the desc file with the new id and PI line added
	if ($desc->{'PI'}){
	    #catenate onto old PI lines
	    $desc->{'PI'}=$oldid."; ".$desc->{'PI'};
	}else{
	    $desc->{'PI'}=$oldid;
	}
	
	#overwrite the DESC file;
	copy($DESC, $DESC.'.old'.$$) if -e $DESC;
	open(DE, "> $DESC") or die "FATAL: failed to open DESC\n[$!]";
	RfamUtils::writeDesc($desc,\*DE);
	close(DE);
	print STDERR "have updated the DESC file with the update PI lines";

	#change acc map ? do we even use this anymore? 
	$db->_move_accession( $acc, $desc->{'ID'} );
	$db->_unlock();
	
    }
}#end of checking PI

#copy files around to correct places
if( &RfamRCS::move_files_to_rcs_directory($acc, $acc) == 0 ) {
    $lock=$rdb->remove_my_lock($user, $lock); 
    die "rfci: Could not move RCS files to directory for family [$acc]. Problem!\n";
}

my $comment;
if( !defined $message ) {
    print "Please give a comment for family [$acc]\n";
    print "Finish comment by a . on the line by itself\n"; 

    while( <STDIN> ) {
	chop;
	/^\s*\.\s*$/ && last;
	$comment .= "$_\n";
    }
} else {
    $comment = $message;
}

if( &RfamRCS::check_in_rcs_files($acc, $comment) == 0 ) {
    $lock=$rdb->remove_my_lock($user, $lock); 
    die "rfci: Could not check in files for $acc. This is a bad error\n";
}

if( &RfamRCS::update_current_directory($acc) == 0 ) {
    $lock=$rdb->remove_my_lock($user, $lock); 
    die "rfci: Could not update directory for $acc\n";
}

#record rcs checkin
$rcs_time=time();
print STDERR "Finished RCS check in\n\n";

#############
#RDB checkin
#############

RDB:
print STDERR "Starting RDB check in\n";

#get entry Obj from RCS
my $en=$db->get_Entry_by_acc($acc);

#this reads in the full scores file
my $full_size=$en->num_seqs_in_full();

print STDERR $full_size, " Seq regions in FULL for $acc\n";

if ($full_size > 6000){
    print STDERR "Doing large family upload for $acc\n";

    eval{
    $rdb->update_wikitext([$en]);
    $rdb->update_rfam([$en]); #full obj change this:
    $rdb->update_rfam_reg_seed( [$en] );

    #dont do lock on this bit-connect not open_transaction
    my $data=$rdb->collate_large_fam_reg_full( [$en] ); #full obj plus sql queries
    if (!$data){
	die "no data from collating rfam-reg_full";
    }
    
    #have to remove this header line or it will kill the upload
    my $auto_rfam=shift(@$data);
    
    #check the rows returned with rows expected in ALIGN
    my $c=scalar(@$data);
    print STDERR "Full align= $full_size and $c rows collated for rfam_reg_full\n";

    #open and write to file-move to correct location and load to RDB
    open(OUT, ">/tmp/$$.rfam.reg.full") || die "cant open the file to write to $!";
    foreach my $r (@{$data}){
	print OUT  "$r\n";
    }
    close OUT;

    #copy file to correct location
    system ("scp /tmp/$$.rfam.reg.full $user\@pfamdb2a:/tmp/") and die "cant copy the file to tmp $!";  
    my $rows=$rdb_admin->load_rfam_reg_full_file_to_rdb("/tmp/$$.rfam.reg.full" , 'rfam_reg_full', $auto_rfam);
    print $rows, " added to the rfam_reg_full table\n";
    $rdb->update_literature_references( [$en] );
    $rdb->update_rfam_database_links( [$en] );
};  if ($@){
    #$lock=$rdb->remove_my_lock($user, $lock) ;
    print STDERR "FAIL to BIG FAM check in $acc: LOCK left on until you deal with it [$@]"; exit;
     }
    
}else{
    print STDERR "Doing small family upload for $acc\n";
    eval{
    $rdb->check_in_Entry( $en );
};  if ($@){
   # $lock=$rdb->remove_my_lock($user, $lock) ;
    print STDERR "FAIL to SMALL FAM check in $acc: LOCK left on until you deal with it [$@]\n"; exit;
    }
  
}

#Successful checkin remove the RDB lock
if ( $lock=$rdb->remove_my_lock($user, $lock) ){
    print STDERR "Failed to remove the lock on RDB after successful checking";
}else{
    print STDERR "Removed lock by $user\n";
}


my $end_time=time();
my $rcs= $rcs_time - $start_time;
my $RCS_ci_time  = RfamUtils::secs2human($rcs);
my $run_time= $end_time - $start_time;             
my $RDB_ci_time  = RfamUtils::secs2human($run_time);
print STDERR "COMPLETED RDB checkin for $acc: Time to check in= ",$RCS_ci_time,"|". $RDB_ci_time, "\n";


######################################
sub help {
    print STDERR <<EOF;
rfci.pl - to check families into rcs and rfamlive RDB

Usage:      rfci.pl -acc RF00000 
	
Options:    MAJOR MODES:
            -updateRDB- if the RCS check in is fine but need to fix the RDB checkin.

To add:
-die if the rqc-all.pl output file is not present

Some bumff on what this code does: 


RCS checkin:
-majority of this is the old rfci.pl code.
-checks RCS database is locked 
-check the accession provided is an existing family (not a new one)
-checks the correct files exist in the RCS for this family
-checks family not already locked by someone else
-checks the sequences/id string etc are ok-doesnt check the seq against rfamseq anymore..
  this was commented out by someone else?? too slow?
-slurps in the DESC file and checks for an AC tag
-all ok so proceed to ci.
-locks RDB 
-checks the id for this acc -if its changed- warns if the new one already used by another family
  if needed it updates the DESC file with a PI line. 
-only places the lock on the family during update of the ID:accmap
-then copies the files around to relevant locations.


RDB checkin:
Only begins when the RCS checkin is complete-majority of method calls are in UpdateRDB.pm
Uses an Entry obj (EntryRCS) to get the data for loading to RDB
I have added the new data onto the Entry obj. Tables to be loaded
    -wikitext- 
    -rfam only one that will update existing data and does not delete first
    -rfam_reg_seed
    _rfam_reg_full
    -rfam_literature_references
    -rfam_database_links

Caveats for a big family greater than 6000 regions in ALIGN:
All works the same as for small families except a different method is used to 
generate and load the data for rfam_reg_full. This is done in two steps (1) the
 collating data stage(slow) does not use a lock as locks time out after 30 mins and this
step may turn out to take longer than this for big fams (it requires one query for each seq).
 I am relying on the _lock table to prevent anyone doing any modifications to the database 
during this data collation. (2) collated data is written to a file in ./, which is copied to 
pfamdb2a/tmp and loaded in. This loading uses a different RDB user/password as the priveleges
 to do this need to be higher. Otherwise the other tables are loaded using the same methods
 as for a small family-except called individually. This does mean that multiple connections are 
made/closed for one check in. However this should only happen for the top 20 large families.


--If its is a small family less than 6000 seqs in full it uses the check_in_Entry which 
  is the way the old code used to work. It runs the methods for all the tables in order, 
  using transactions-which puts locks on all the tables and then does not commit data if
  an error has occurred. 

-remove lock flag on rdb

EOF
}
