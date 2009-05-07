#!/software/bin/perl
#
# This script allows one to check in a family to the SVN repository containing Pfam families.
# The family that you wish to checkout should be passed in as a parameter
#

use strict;
use warnings;
use Cwd;
use Data::Dumper;
use Getopt::Long;

use Bio::Pfam::SVN::Client;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamQC;

#-------------------------------------------------------------------------------
# Deal with all of the options

my ( $message, $ignore, $onlydesc, $addToClan, $removeFromClan, $help );

&GetOptions(
  "m=s"              => \$message,
  "i"                => \$ignore,
  "onlydesc"         => \$onlydesc,
  "add_to_clan"      => \$addToClan,
  "remove_from_clan" => \$removeFromClan,
  "help"             => \$help
);

my $family = shift;
chomp($family);

unless ($family) {
  warn "\n***** No family passed  *****\n\n";
  help();
}

if (@ARGV) {
  warn "\n***** $0 no longer supports multiple family check-ins *****\n\n";
  help();
}

if ($iterated) {
  warn "\n***** -iterated not current supported *****\n\n";
  help();
}

if ( $removeFromClan and $addToClan ) {
  warn
"\n***** You cannot use the -add_family and -remove_family options together *****\n\n";
}

help() if ($help);

#-------------------------------------------------------------------------------
my $pwd = getcwd;

if ( !( -d "$pwd/$family" ) ) {
  die
"$0: [$pwd/$family] is not a current directory.\nMust be in the parent directory of the family to check in\n";
}

if ( !-w "$pwd/$family" ) {
  die
    "$0: I can't write to directory [$pwd/$family].  Check the permissions.\n";
}

if ( !( -d "$pwd/$family/.svn" ) ) {
  die
"$0: [$pwd/$family] does not look like that it is from the Pfam subversion repository";
}

#-------------------------------------------------------------------------------
# If a message is supplied, then write it to file such that the code reference
# that deals with the SVN log message can grab it.

if ( -s ".defaultpfci" ) {
  unlink(".defaultpfci")
    or die "Could not remove old default check-in message\n";
}

if ($message) {
  open( M, ">.defaultpfci" ) or die "Could not open message file\n";
  print M $message;
  close(M);
}

#-------------------------------------------------------------------------------
#Initial SVN stuff

#Check that family exists in svn
my $client = Bio::Pfam::SVN::Client->new;
$client->checkFamilyExists($family);

#Need to check that the family is up-to-date

#
#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware

my $familyIO = Bio::Pfam::FamilyIO->new;

if ($onlydesc) {
  $client->addPFANNLog();

  #AC

  #my $upFamObj = $familyIO->loadPfamADESCFromLocalFile($family, $pwd);
  print STDERR "Successfully loaded $family through middleware\n";

#TODO - add this QC step back in!
  #Check the desc accessions are the same
  #if($upFamObj->DESC->AC ne $oldFamObj->DESC->AC){
  #  die "Accession error, your local copy does not match the repository\n";
  #}

}
else {
  
  if ( !Bio::Pfam::PfamQC::checkFamilyFiles($family) ) {
    print "pfci: $family contains errors.  You should rebuild this family.\n";
    exit(1);
  }
  my $upFamObj = $familyIO->loadPfamAFromLocalFile( $family, $pwd );
  print STDERR "Successfully loaded $family through middleware\n";

#-------------------------------------------------------------------------------

  if ($addToClan) {

    #Check that the clan accession is in the correct format.
    unless ( $upFamObj->DESC->CL =~ /CL\d{4}/ ) {
      die "$addToClan does not look like a clan accession\n";
    }
    $client->checkClanExists($upFamObj->DESC->CL);
    my $clanIO     = Bio::Pfam::ClanIO->new;
    my $clanObj    = $clanIO->loadClanFromSVN($upFamObj->DESC->CL);
    my %membership = map { $_ => 1 } @{ $clanObj->DESC->MEMB };
    if ( $membership{ $upFamObj->DESC->AC } ) {
      die
"Trying to add $family to $addToClan, yet it appears that $family is already part of the clan\n";
    }
    
    #Need to propergate the fact we want to add this family to a clan via the log message.
    open(C, ">.atc") or die "Could not open .atc:[$!]\n";
    print C $newFamObj->DESC->CL.":".$newFamObj->DESC->AC;
    close(C);
    
    $client->addPFCIATCLog();
  }
  elsif ($removeFromClan) {

    #Check that the clan accession is in the correct format.
    unless( $oldFamObj->DESC->CL =~ /CL\d{4}/ ) {
      die "$removeFromClan does not look like a clan accession\n";
    }
    
#TODO
    #unless(Bio::Pfam::PfamQC::noMissing($upFamObj, $oldFamObj, $family )){
    #  exit(1);   
    #}

    #Does the clan in question exist?
    $client->checkClanExists($removeToClan);

    #Seems like it does, lets load it!
    my $clanIO  = Bio::Pfam::ClanIO->new;
    my $clanObj = $clanIO->loadClanFromSVN($oldFamObj->DESC->CL);

    #Now check the membership.
    my %membership = map { $_ => 1 } @{ $clanObj->DESC->MEMB };
    unless ( $membership{ $upFamObj->DESC->AC } ) {
      die
"Trying to remove $family from $removeFromClan, yet it does not appear that the $family is part of the clan\n";
    }
    
    open(C, ">.rmc") or die "Could not open .rmc:[$!]\n";
    print C $oldFamObj->DESC->CL.":".$oldFamObj->DESC->AC;
    close(C);
    
    $client->addPFCIRMCLog();
  }else{
    $client->addPFCILog();  
  }

}

#my $oldFamObj = $familyIO->loadPfamAFromSVN($family, $client);
my $oldFamObj;
print STDERR "Successfully loaded remote $family through middleware\n";

#AC present
#if($upFamObj->DESC->AC ne $oldFamObj->DESC->AC){
#  die "Accession error, your local copy does not match the repository\n";
#}

#These are more sanity checks
unless ($ignore) {

  #If we are at sanger, perform an overlap check against the database.
  if ( $config->location eq "WTSI" ) {
    my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{$connect} );

    #Find out if family is in rdb
    my $rdb_family = $pfamDB->getPfamData($family);
    my $overlaps =
      &Bio::Pfam::PfamQC::family_overlaps_with_db( $family, \%ignore, undef,
      $pfamDB, $upFamObj );
    if ($overlaps) {
      print "Looks like your family contains overlaps.\n";
      exit(1);
    }
  }

  unless ( Bio::Pfam::PfamQC::sequenceChecker( $family, $upFamObj ) ) {
    print "pfci: $family contains errors.  You should rebuild this family.\n";
    exit(1);
  }

  #unless(Bio::Pfam::PfamQC::noMissing($upFamObj, $oldFamObj, $family )){
  #  exit(1);
  #}

  #pqc-check $family

  unless ( Bio::Pfam::PfamQC::noFragsInSeed( $family, $upFamObj ) ) {
    exit(1);
  }

  unless ( Bio::Pfam::PfamQC::nonRaggedSeed( $family, $upFamObj ) ) {
    exit;
  }
}

#NEED TO CHECK THAT ASSURTIONS COVER ALL FORMAT CHECKS.....
unless ( Bio::Pfam::PfamQC::passesAllFormatChecks( $upFamObj, $family ) ) {
  exit(1);
}


#-------------------------------------------------------------------------------
#If we get here, then great! We can now check the family in!
my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

$client->commitFamily($family);

#Remove any file containing the check-in message
if ( -s ".defaultpfci" ) {
  unlink(".defaultpfci")
    or die "Could not remove old default check-in message\n";
}

#
if ($caught_cntrl_c) {
  print STDERR
"\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** but this could be very bad.  You really must tell someone about this!\n";
}

exit(0);

#-------------------------------------------------------------------------------
# SUBROUTINES ------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 subname 

  Title    :
  Usage    :  
  Function :
  Args     :
  Returns  :
  
=cut

sub checkClanOpts {
  if ($addToClan) {

    #Check that the clan exists.

    #Check that the family is not already part of clan.
  }

  if ($removeFromClan) {

    #Check that the clan exists.

    #Check that the family is part of the clan

  }
}
