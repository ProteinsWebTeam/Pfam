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
use Bio::Pfam::PfamLiveDBManager;

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
) or die "Unrecognised option passed in to the script.\n";

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

if ( $removeFromClan and $addToClan ) {
  warn
"\n***** You cannot use the -add_family and -remove_family options together *****\n\n";
  help();
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

if ( -s ".default" . $$ . "pfci" ) {
  unlink( ".default" . $$ . "pfci" )
    or die "Could not remove old default check-in message\n";
}

if ($message) {
  open( M, ">.default" . $$ . "pfci" ) or die "Could not open message file\n";
  print M $message;
  close(M);
}

#-------------------------------------------------------------------------------
#Initial SVN stuff

#Check that family exists in svn

my $config = Bio::Pfam::Config->new;
my $client = Bio::Pfam::SVN::Client->new;
$client->checkFamilyExists($family);

#Need to check that the family is up-to-date

#
#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware
my $familyIO = Bio::Pfam::FamilyIO->new;

my ( $oldFamObj, $upFamObj );

if ($onlydesc) {

  print STDERR "Only considering the DESC file\n";
  $upFamObj = $familyIO->loadPfamAFromLocalFile( $family, $pwd );
  print STDERR "Successfully loaded local copy $family through middleware\n";
  $oldFamObj = $familyIO->loadPfamAFromSVN( $family, $client );
  print STDERR "Successfully loaded SVN copy of $family through middleware\n";
}
else {

  if ( !Bio::Pfam::PfamQC::checkFamilyFiles($family) ) {
    print "pfci: $family contains errors.  You should rebuild this family.\n";
    exit(1);
  }

  $upFamObj = $familyIO->loadPfamAFromLocalFile( $family, $pwd );
  print STDERR "Successfully loaded local copy $family through middleware\n";
  $oldFamObj = $familyIO->loadPfamAFromSVN( $family, $client );
  print STDERR "Successfully loaded SVN copy of $family through middleware\n";
}

#-------------------------------------------------------------------------------
#Common QC checks based on the user request flags.

if ($addToClan) {
  print STDERR "Checking clan information\n";

  #When adding to a clan, we want to check that the family
  #does not already have a CL line in the old DESC, and that the
  #new DESC has a CL line and that the clan exists.

  #As part of belt and brace, we also check that the family is
  #not already part of the clan membership (some how)

  #Check that the clan accession is in the correct format.
  unless ( defined($upFamObj->DESC->CL) and $upFamObj->DESC->CL =~ /CL\d{4}/ ) {
    die "CL line in current family is either not defined or does not look like a clan accession\n";
  }
  $client->checkClanExists( $upFamObj->DESC->CL );
  my $clanIO = Bio::Pfam::ClanIO->new;
  my $clanObj = $clanIO->loadClanFromSVN( $upFamObj->DESC->CL, $client );
  my %membership;
  if ( $clanObj->DESC->MEMB ) {
    %membership = map { $_ => 1 } @{ $clanObj->DESC->MEMB };
  }
  if ( $membership{ $upFamObj->DESC->AC } ) {
    die
"Trying to add $family to $addToClan, yet it appears that $family is already part of the clan\n";
  }
  if ( $oldFamObj->DESC->CL and $oldFamObj->DESC->CL =~ /CL\d{4}/ ) {
    die
"You have asked for this family to be added to a clan, but it is already part of a clan!\n"
      . "Found:"
      . $oldFamObj->DESC->CL
      . " in SVN copy\n";
  }

#Need to propergate the fact we want to add this family to a clan via the log message.
  open( C, ">.atc" . $$ ) or die "Could not open .atc:[$!]\n";
  print C $upFamObj->DESC->CL . ":" . $upFamObj->DESC->AC;
  close(C);

  $client->addPFCIATCLog();
}
elsif ($removeFromClan) {
  print STDERR "Checking clan information\n";

  #Things to check:
  # - Does the clan exist?
  # - Is the family part of a clan?
  # - That the CL line has been removed
  # - Ensure that CL line has been removed from the file

  #Check that the clan accession is in the correct format.
  unless ( defined($oldFamObj->DESC->CL) and $oldFamObj->DESC->CL =~ /CL\d{4}/ ) {
    die "SVN copy has:|"
      . ( defined($oldFamObj->DESC->CL) ? $oldFamObj->DESC->CL : "")
      . "| but this does not look like a clan accession\n";
  }

  #Does the clan in question exist?
  $client->checkClanExists( $oldFamObj->DESC->CL );

  #Seems like it does, lets load it!
  my $clanIO = Bio::Pfam::ClanIO->new;
  my $clanObj = $clanIO->loadClanFromSVN( $oldFamObj->DESC->CL, $client );

  if ( $upFamObj->DESC->CL and $upFamObj->DESC->CL =~ /CL\d{4}/ ) {
    die "local copy has:|"
      . $upFamObj->DESC->CL
      . "| yet you have asked to remove it from the clan.  Remove CL line and try again.\n";
  }

  #Now check the membership.
  my %membership = map { $_ => 1 } @{ $clanObj->DESC->MEMB };
  unless ( $membership{ $upFamObj->DESC->AC } ) {
    die
"Trying to remove $family from $removeFromClan, yet it does not appear that the $family is part of the clan\n";
  }

  open( C, ">.rmc" . $$ ) or die "Could not open .rmc:[$!]\n";
  print C $oldFamObj->DESC->CL . ":" . $oldFamObj->DESC->AC;
  close(C);

  $client->addPFCIRMCLog();
}
else {
  $client->addPFCILog();
}

#Check the desc accessions are the same
if ( $upFamObj->DESC->AC ne $oldFamObj->DESC->AC ) {
  die
"\n***** Accession error, your local copy does not match the SVN repository *****\n\n";
}

#Check the desc accessions are the same
if ( $upFamObj->DESC->ID ne $oldFamObj->DESC->ID ) {
  die
"\n***** Identifier error, your local copy does not match the SVN repository. "
    . " Do you mean to change the name? If so use pfmove. *****\n\n";
}

if ( $oldFamObj->DESC->CL ) {
  if ( $upFamObj->DESC->CL ) {
    unless ( $upFamObj->DESC->CL eq $oldFamObj->DESC->CL ) {
      die
"The clan acession in the CL has been changed between the SVN copy and your local copy!:"
        . "From:"
        . $oldFamObj->DESC->CL . " to "
        . $upFamObj->DESC->CL
        . ".  You can not do this!\n"
        . "(Use pfci with the remove_from_clan option followed by add_to_clan)\n";
    }
  }
  else {
    unless ($removeFromClan) {
      die
"The SVN copy of this family has a CL line, but this version does not. Either use the -remove_from_clan option or put back.\n";
    }
  }
}

#Need to check that if the updated family does have a CL line and that the SVN copy is the same
#unless the add_to_clan flag is used.
if ( $upFamObj->DESC->CL ) {
  unless ($addToClan) {
    unless ( $oldFamObj->DESC->CL
      and $upFamObj->DESC->CL eq $oldFamObj->DESC->CL )
    {
      die
"Found a CL line in this DESC, but it is either not present in the SVN copy of different."
        . " Did you mean to use -add_to_clan?\n";
    }
  }
}

#-------------------------------------------------------------------------------

if ($onlydesc) {

  #Check that none of the lines that should not be touch are not

  #The list of fields  that cannot be alter are:
  # NC, TC, GA,
  foreach my $tag (qw(NC TC GA)) {
    my $ftag = "CUT" . $tag;
    unless ( $oldFamObj->DESC->$ftag->{seq} eq $upFamObj->DESC->$ftag->{seq}
      and $oldFamObj->DESC->$ftag->{dom} eq $upFamObj->DESC->$ftag->{dom} )
    {
      die
"There is a differnce in your $tag lines between what is in the SVN repository and this local copy.".
        " You can not do this when only commint a DESC file!\n";
    }
  }

  if ( $oldFamObj->DESC->EDITS ) {
    for ( my $i = 0 ; $i < scalar( $oldFamObj->DESC->EDITS ) ; $i++ ) {

    }
  }

  #ID, AC, PI, SE, BM, SM
  foreach my $tag (qw(ID AC PI SE BM SM)) {
    next unless(defined($oldFamObj->DESC->$tag));
    unless ( $oldFamObj->DESC->$tag eq $upFamObj->DESC->$tag ) {
      die
"You are only checking in the DESC file, yet the $tag line has change. You can not do this!\n";
    }
  }
}

#These are more sanity checks
unless ($ignore) {

#Regardless of whether we are just checking in a DESC or the complete set of family files.
  Bio::Pfam::PfamQC::checkDESCSpell( $family, $familyIO );

  unless ($onlydesc) {

    #If we are at sanger, perform an overlap check against the database.
    if ( $config->location eq "WTSI" ) {
      my $connect = $config->pfamlive;
      my $pfamDB  = Bio::Pfam::PfamLiveDBManager->new( %{$connect} );

      #Find out if family is in rdb
      my $rdb_family = $pfamDB->getPfamData($family);

      my %ignore;

      #Need to populate the ignore hash with clan and nesting data......

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

    unless ( Bio::Pfam::PfamQC::noMissing( $upFamObj, $oldFamObj, $family ) ) {
      print(
"$0: your family seems to be missing members compared to the DBN copy\n(see $family/missing). Please inspect loss of members.\n"
      );
      print("Do you want to continue regardless? [y/n]  ");
      my $reply = <STDIN>;
      chomp $reply;
      if ( $reply ne "y" ) {
        exit(1);
      }
    }

    #pqc-check $family

    unless ( Bio::Pfam::PfamQC::noFragsInSeed( $family, $upFamObj ) ) {
      exit(1);
    }

    unless ( Bio::Pfam::PfamQC::nonRaggedSeed( $family, $upFamObj ) ) {
      exit;
    }

    #NEED TO CHECK THAT ASSURTIONS COVER ALL FORMAT CHECKS.....
    unless ( Bio::Pfam::PfamQC::passesAllFormatChecks( $upFamObj, $family ) ) {
      exit(1);
    }
  }
}

#-------------------------------------------------------------------------------
#If we get here, then great! We can now check the family in!
my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

if ($onlydesc) {
  $client->commitFamilyDESC($family);
}
else {
  $client->commitFamily($family);
}

#Remove any file containing the check-in message
if ( -s ".default" . $$ . "pfci" ) {
  unlink( ".default" . $$ . "pfci" )
    or die "Could not remove old default check-in message\n";
}

#Remove any file containing the check-in message
if ( -s ".rmc" . $$ ) {
  unlink( ".rmc" . $$ )
    or die "Could not remove old remove from clan check-in message\n";
}
if ( -s ".atc" . $$ ) {
  unlink( ".atc" . $$ )
    or die "Could not remove old add to clan check-in message\n";
}

#
if ($caught_cntrl_c) {
  print STDERR
"\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** but this could be very bad.  You really must tell someone about this!\n";
}
exit(0);


sub help {
  
print<<EOF;

  usage: $0 <directory>
  
  Where the directory contains the files that consitute a Pfam-A entry.
  
  Aim: To perform quality control checks on an existing family and commit to the SVN repository.
  
  -add_to_clan      - Add the family to the clan specified on the CL line of the DESC file. This 
                      flag/option only needs to be added the first time the CL line is added.
  -remove_from_clan - Remove the clan family from the clan. The CL line should have already been
                      deleted from the DESC file.
  -onlydesc         - Speeds up check-ins a bit by avoiding the QC and onyl updating the contents
                      in the DESC file.  Do not change DESC contents that affect the other files,
                      particularly cut-offs.  Useful for when making updates to annotation or 
                      adding a family to a clan. If you are uncertain of what you are doing, do 
                      not use this option.
  -i                - Ignore some of the QC steps to speed up check-in.
  -m                - Specify the message that describes the changes you have made to this family 
                      on the command line, avoid being prompted for it at a later satge.                   
                      
EOF

exit(1);

}
