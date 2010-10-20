#!/software/bin/perl 

#A script to check a new family into the database & RCS.

#1. Parse and validate a CLANDESC file in the given directory.
#2. Assign a unique and stable accession.  
#3. Load the data into the rfamlive DB.
#4. Check the file into CVS.

#Lots of this has been copied from rfnew.pl!

use warnings;
use strict;

#Rfam modules:
use RfamRCS;
use Rfam::Clans::Clan;

if( $#ARGV == -1 ) {
    print "Usage: rfclnew.pl <dir> -- where dir is a directory containing a legal CLANDESC file!\n";
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

my $acc = Rfam::Clans::Clan::id_exists( $id );

print "Checking in: [$id] [$acc]\n";

#Die if anything looks dodgy!
die "rfclnew: [$id] is not a current directory.\nMust be in the parent directory of the family to make a new family.\n" if not -d $id;
die "FATAL: rfclnew: clandesc file does not exist or is empty [./$id/CLANDESC]. [$!]" if not -s "./$id/CLANDESC";
#Uses rfamlive database -- also check flatfiles?
die "rfclnew: Family [$id] already exists.\nIf this is an existing family, check in the revision using rfclci.pl\n" if $acc;

#Read in clandesc file:
my $desc = Rfam::Clans::Clan::slurpClanDesc("./$id/CLANDESC");

#Allocate a new stable accession:
$acc = Rfam::Clans::Clan::allocate_new_clan_accession($id);
die "rfclnew: Unable to allocate new accession number. Check write permission to ACCESSION dir\n" if not defined $acc;
$desc->{'AC'} = $acc;
$desc->{'ID'} = $id;

#Create a directory to write to and write clandesc:
Rfam::Clans::Clan::make_new_clan_files($desc);

#check into cvs repository:
#my $comment = "New clan";

Rfam::Clans::Clan::check_into_DB($desc);


#Rfam::Clans::Clan::make_view_files($acc);

#print STDERR "\n\nChecked in family [$acc]\n";

exit(0);



