#!/software/bin/perl -w

#
# author sgj
# modified heavily by jd7 to use new rfci code 
#

#BEGIN {
#   $rfam_mod_dir = "/software/rfam/scripts/Modules/";
#}

#use lib $rfam_mod_dir;
use strict;
use Rfam;
use RfamQC;
use RfamRCS;
use Getopt::Long;
use Rfam::UpdateRDB;
use DBI;
use DBD::mysql;
use vars qw($opt_a);
use DateTime;
use RfamUtils;
use Cwd;
use File::Copy;


my ($id, 
    $message,
    $help);

&GetOptions(
	    '-id=s'=> \$id,
	    '-m:s' => \$message,
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

#check ID and lock #################

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

# check is new family
if( ! $id ) {
    die "rfnew: you must specify the name of the directory to checkin use `-id` option \n";
}

if( $id =~ /\/$/ ) {
    $id =~ s/\/$//g;
}

if(   !(-d $id) ) {
    die "rfnew: [$id] is not a current directory.\nMust be in the parent directory of the family to make a new family\n";
}

if( &RfamQC::id_exists( $id ) ) {
    die "rfnew: Family [$id] already exists.\nIf this is an existing family, check in the revision using rfci.pl\n";
}
###############################
#check for the qc.passed file

my $dir=cwd;

#need to add in this for checking the qc has been done-dont allow ci otherwise.
    if (! -e "$dir/$id/qcpassed" ){
	die "rfci: [$id] has not been passed by qc checks so not ready to check in - run rqc-all.pl\n";
    }
#check qc.passed is older than the relevant files
 
 if( (-M "$dir/$id/DESC"      < -M "$dir/$id/qcpassed") ||
     (-M "$dir/$id/SEED"      < -M "$dir/$id/qcpassed") ||
     (-M "$dir/$id/OUTPUT"    < -M "$dir/$id/qcpassed") ||
     (-M "$dir/$id/out.list"  < -M "$dir/$id/qcpassed")
    ) {
        die "You need to rerun the rqc-all.pl as at least one of DESC/SEED/OUTPUT/out.list has changed since you ran it last\n";
 }

################################

if( ! open(DESC,"./$id/DESC") ) {
    die "rfnew: cannot open the desc file [./$id/DESC]. [$!]";
}

while(<DESC>) {
    chomp;
    if( /^AC/ ) {
	die "rfnew: Your DESC file has a AC line [$_].\n";
    }
    if( /^ID/ ) {
	#THIS IS DUMB!
	die "rfnew: Your DESC file has a ID line [$_].\n";
    }
    
   }
close(DESC);

my $comment = "New family";

unless( &RfamQC::valid_sequences( $id ) ) {
    die "rfnew: Your sequences don't all match the database\n";
}

########################

#RCS lock & update ################

my $acc = &RfamRCS::allocate_new_accession($id);
if( !defined $acc ) {
    die "rfnew: Unable to allocate new accession number. Check write permission to ACCESSION dir\n";
}

if( ! &RfamRCS::make_new_rcs_directory($acc) ) {
    die "rfnew: Cannot make a new directory for $id.\nCheck you have write permissions to RCS_MASTER\n";
}

open(LOCK,">$Rfam::rcs_master_dir/$acc/locked") or die "rfnew: cannot write to lock file\n";
print LOCK "First lock due to rfnew\n";
close(LOCK);

if( !open(DESC,"./$id/DESC") ) {
    die "rfnew: cannot open the desc file. Yikes!";
}

if( !open(TEMP,">./$id/DESC.$$") ) {
    die "rfnew: cannot open a desc file to write to. Yikes!";
}

print TEMP "AC   $acc\n";
print TEMP "ID   $id\n";
while(<DESC>) {
    print TEMP;
}
close(DESC);
close(TEMP);

if( !rename("$id/DESC.$$","$id/DESC") ) { 
    die "rfnew: Could not rename temp desc file [$!]";
}

if( &RfamRCS::move_files_to_rcs_directory($acc,$id) == 0 ) {
    die "rfnew: Could not move RCS files to directory for family [$acc]. Problem!\n";
}

if( &RfamRCS::make_new_rcs_files($acc,$comment) == 0 ) {
    die "rfnew: Could not make new files for family [$acc]. Problem!\n";
}

my $date = gmtime();
$comment = "family [$acc] deposited on $date";

if( &RfamRCS::check_in_rcs_files($acc,$comment) == 0 ) {
    die "rfnew: could not check in files for family $acc. Problem!\n";
}

if( &RfamRCS::update_current_directory($acc) == 0 ) {
    die "rfnew: Could not update current directory for $acc\n";
}

#record rcs checkin
$rcs_time=time();
print STDERR "Finished RCS check in\n\n";

###########################################

print STDERR "Starting RDB check in new family\n";

#All is apparently ok-proceed with a RCS checkin

#RCS database
my $db = Rfam::default_db();

#Rfamlive database
my $rdb = Rfam::live_rdb_update(); 
my $rdb_admin=Rfam::live_rdb_update_admin();

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

    #open and write to file
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
    print STDERR "FAIL to BIG FAM check in $acc: LOCK left on until you deal with it [$@]"; exit;
     }
    
}else{
    print STDERR "Doing small family upload for $acc\n";
    eval{
    $rdb->check_in_Entry( $en );
};  if ($@){
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
print STDERR "COMPLETED checkin for $acc: Check in: RCS= ",$RCS_ci_time, " | RDB= ", $RDB_ci_time, "\n";

exit(0);

