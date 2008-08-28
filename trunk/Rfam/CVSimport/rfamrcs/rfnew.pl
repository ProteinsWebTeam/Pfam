#!/software/bin/perl -w

#
# author sgj
# heavily stolen from Ewan's pfnew
#

use strict;
use Rfam;
use RfamQC;
use RfamRCS;
use Getopt::Long;
use Rfam::UpdateRDB;
use DBI;

if( $#ARGV == -1 ) {
    &RfamRCS::show_rcs_help(\*STDOUT);
    exit;
}

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

my $id = shift;

if( $id =~ /\/$/ ) {
    $id =~ s/\/$//g;
}


if( !(-d $id) ) {
    die "rfnew: [$id] is not a current directory.\nMust be in the parent directory of the family to make a new family\n";
}

if( &RfamQC::id_exists( $id ) ) {
    die "rfnew: Family [$id] already exists.\nIf this is an existing family, check in the revision using rfc
i\n";
}

if( ! open(DESC,"./$id/DESC") ) {
    die "rfnew: cannot open the desc file [./$id/DESC]. [$!]";
}

my $wikipedia;
while(<DESC>) {
    chomp;
    if( /^AC/ ) {
	die "rfnew: Your DESC file has a AC line [$_].\n";
    }
    if( /^ID/ ) {
	#THIS IS DUMB!
	die "rfnew: Your DESC file has a ID line [$_].\n";
    }
    
    if (/^WK\s+(.+)/){
	$wikipedia = $1;
	#Should add some verification that this is a bona-fide wikipedia link:
	#Strip off irrelevant info:
	$wikipedia =~ s/en.wikipedia.org\/wiki\///;
	$wikipedia =~ s/http:\/\///;
	$wikipedia =~ s/\;//;
    }
}
close(DESC);

if (!defined($wikipedia)){
    $wikipedia = 'Rfam';
    print "WARNING: a wikipedia entry was not given. Using default ($wikipedia).\n"
}

my $comment = "New family";

unless( &RfamQC::valid_sequences( $id ) ) {
    die "rfnew: Your sequences don't all match the database\n";
}

#
# Ok. Attempt to get the lock. If we fail a rather ungraceful exit.
# 

# dunno why we were doing this - allocate_new_accession gets the lock

#my $ret = &RfamRCS::get_accession_lock();

#if( !($ret =~ /^success/ ) ) {
#    die "rfnew: The accession lock has been grabbed by [$ret].\nAccession locking should be short - try again in a couple of minutes\n";
#}

my $acc = &RfamRCS::allocate_new_accession($id);
if( !defined $acc ) {
#    &RfamRCS::release_accession_lock();
    die "rfnew: Unable to allocate new accession number. Check write permission to ACCESSION dir\n";
}

if( ! &RfamRCS::make_new_rcs_directory($acc) ) {
#    &RfamRCS::release_accession_lock();
    die "rfnew: Cannot make a new directory for $id.\nCheck you have write permissions to RCS_MASTER\n";
}

open(LOCK,">$Rfam::rcs_master_dir/$acc/locked") or die "rfnew: cannot write to lock file\n";
print LOCK "First lock due to rfnew\n";
close(LOCK);

#&RfamRCS::release_accession_lock();

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

# rdb stuff when its ready

## READY NOW!!! i hope

print STDERR "\nChecking family into RDB\n";

eval {
  my $rdb =  Rfam::live_rdb_update();
  my $db = Rfam::default_db();
  my $en = $db->get_Entry_by_acc( $acc);
  my $id = $en->author();
  $rdb->check_in_Entry( $en );
  make_wiki_entry($acc,$wikipedia);
};

$@ and do {
  print STDERR "rfnew: RDB update; Could not update relational database for family $acc [$@]\n";
};
print STDERR "RDB update succesful\n";

&RfamRCS::make_view_files($acc); 

print STDERR "\n\nChecked in family [$acc]\n";

exit(0);

######################################################################
#Nasty add on to fill the wiki table for the entry:
sub make_wiki_entry {
    my $wacc = shift;
    my $wiki = shift;

    my $rfquery = qq(
select auto_rfam from rfam where rfam_acc = '$wacc'
);

# MySQL rfamlive connection details.
    my $rfdatabase = "rfamlive";
    my $rfhost     = "pfamdb2a";
    my $rfuser     = "pfamadmin";
    my $rfpw       = "mafpAdmin";
    my $rfport     = 3303;

    my ($rfdbh, $rfsth);
# Create a connection to the database.
    $rfdbh = DBI->connect(
	"dbi:mysql:$rfdatabase:$rfhost:$rfport", $rfuser, $rfpw, {
	    PrintError => 1, #Explicitly turn on DBI warn() and die() error reporting. 
	    RaiseError => 1
	}    );
     
# Prepare the query for execution.
    $rfsth = $rfdbh->prepare($rfquery);
    $rfsth->execute();
          
    my($temp_auto) = $rfsth->fetchrow;
    $rfsth->finish();
    my $rdb_auto_num = $temp_auto if(defined($temp_auto)); 
     
    $rfquery = "
insert into wiki (auto_rfam, rfam_acc, title) VALUES ($rdb_auto_num,'$wacc','$wiki')
";
    print "Prepare:\n$rfquery\n";
    $rfsth = $rfdbh->prepare($rfquery);
    $rfsth->execute();
    $rfdbh->disconnect;
}


