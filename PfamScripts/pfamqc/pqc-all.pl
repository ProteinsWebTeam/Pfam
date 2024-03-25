#!/usr/bin/env perl
#
# Script to run all QC checks at a given location.
#

use strict;
use warnings;
use Cwd;
use Getopt::Long;

use Bio::Pfam::SVN::Client;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamQC;
use Bio::Pfam::PfamLiveDBManager;

my ( $verbose, $help );

&GetOptions(
  "verbose" => \$verbose,
  "help"    => \$help,
);

help() if ($help);

my $family = shift;

unless($family){
  print STDERR "\n*** No family specified ***\n\n";
  help();
}

$verbose and print STDERR "Going to run PfamQC methods on $family\n";

my $config = Bio::Pfam::Config->new;

#Check that family exists in svn
my $client = Bio::Pfam::SVN::Client->new;
my $pwd = getcwd;

#-------------------------------------------------------------------------------
#First QC step is to check the timestamps on the files

if ( !Bio::Pfam::PfamQC::checkFamilyFiles($family) ) {
  print STDERR "$0: $family contains errors.  You should rebuild this family.\n";
  exit(1);
}

$verbose and print STDERR "$0: All family files are present\n";



#
#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware

my $familyIO = Bio::Pfam::FamilyIO->new;
my $famObj = $familyIO->loadPfamAFromLocalFile( $family, $pwd );
$verbose and print STDERR "Successfully loaded $family through middleware\n";

#------------------------------------------------------------------------------------
#Check user has filled in all the fields in DESC file
#Could do this bit just for new families, but doing it for all families just in case
my $user_fields;
if($famObj->DESC->ID eq "ShortName") {
  print STDERR "Need to change the id in the DESC file (currently 'ShortName')\n";
  $user_fields=1;
} 
if($famObj->DESC->DE eq "Family description") {
  print STDERR "Need to change the description in the DESC file (currently 'Family description')\n";
  $user_fields=1;
} 
if($famObj->DESC->AU eq "Who RU") {
  print STDERR "Need to change the author name in the DESC file (currently 'Who RU')\n";
  $user_fields=1;
}
if($famObj->DESC->SE eq "Where did the seed come from") {
  print STDERR "Need to change the seed source in the DESC file (currently 'Where did the seed come from')\n";
  $user_fields=1;
}
print "\n" if($user_fields);


#-------------------------------------------------------------------------------
#Check DESC file for ID/AC and that if we have a CL line that we really meant it

my ($isNew, $svnFamObj);
unless ( $famObj->DESC->ID  and $famObj->DESC->ID =~ /\S+/   ) {
  die
"$0: Your family does not appear have an identifier!  The name of the family is now "
    . "supplied in the DESC file and families are stored under their accession\n."
    . "The check-in process will automatically assign the accession and position it in the
  repository for you!\n";

}

#directory for svn family object
my $dir = File::Temp->newdir( 'CLEANUP' => 1 ); 

if ( $famObj->DESC->AC ) {
  $isNew = 0;
  
  $verbose and  print STDERR "$0: Found an accession, assuming the family already exists in the repository\n";
  
  #Check that the family exists in the repository
  $client->checkFamilyExists($famObj->DESC->AC); 
 
  $svnFamObj= $familyIO->loadPfamAFromSVN($famObj->DESC->AC, $dir, $client);
  
  unless(defined($svnFamObj) and $svnFamObj->isa('Bio::Pfam::Family::PfamA')){
    die "$0: Failed to load family from SVN $svnFamObj\n";  
  }
  
  #Check that the ID/ACs are the same
   #Check the desc accessions are the same
  if ( $svnFamObj->DESC->AC ne $famObj->DESC->AC ) {
    die "$0: Accession error, your local copy does not match the repository\n";
  }

  #Check the desc accessions are the same
  if ( $svnFamObj->DESC->ID ne $famObj->DESC->ID ) {
    die "$0: Identifier error, your local copy does not match the repository\n";
  }


}
else {
  $isNew = 1;

  #-------------------------------------------------------------------------------
  #Check that the pending family does ot already exist. Parnoid check as new families
  #should be removed immediately into the main respository.

  $client->checkNewFamilyDoesNotExists( $famObj->DESC->ID );
  
}

unless ( $famObj->DESC->ID  and $famObj->DESC->ID =~ /\S+/   ) {
  die
"$0: Your family does not appear have an identifier!  The name of the family is now "
    . "supplied in the DESC file and families are stored under their accession\n."
    . "The check-in process will automatically assign the accession and position it in the
  repository for you!\n";
}



if ( $famObj->DESC->CL ) {
  #Check clan is in the SVN repository.
  $verbose and print STDERR "$0: Found a clan cross references in you DESC file, checking\n";
  my $clanIO = Bio::Pfam::ClanIO->new;
  my $clanObj = $clanIO->loadClanFromSVN($famObj->DESC->CL, $client);
  
  unless(defined($clanObj) and $clanObj->isa('Bio::Pfam::Clan::Clan')){
    die "$0: Found a clan cross reference, but failed to load the Clan from the SVN repository\n";
  }
  
}

my $pfamDB;
if ( $config->location eq 'WTSI' or $config->location eq 'EBI' ) {
  my $connect = $config->pfamlive;
  $pfamDB  = Bio::Pfam::PfamLiveDBManager->new( %{$connect} );
}

#NEED TO CHECK THAT ASSURTIONS COVER ALL FORMAT CHECKS.....
unless ( Bio::Pfam::PfamQC::passesAllFormatChecks( $famObj, $family, undef, undef, $pfamDB ) ) {
  exit(1);
}


#If we are at sanger/ebi, perform an overlap check against the database.
if ( $config->location eq 'WTSI' or $config->location eq 'EBI' ) {

  #Find out if family is in rdb
  my $rdb_family = $pfamDB->getPfamData($family);

  unless ( Bio::Pfam::PfamQC::sequenceChecker( $family, $famObj, $pfamDB ) ) {
    print "$0: $family contains errors.  You should rebuild this family.\n";
    exit(1);
  }

  #Need to populate the ignore hash with clan and nesting data......
  my %ignore;

  my $overlaps =
    &Bio::Pfam::PfamQC::family_overlaps_with_db( $family, \%ignore, $pfamDB, $famObj, undef, undef);
  
  if ($overlaps) {
    print "$0: Looks like your family contains overlaps.\n";
    exit(1);
  }
  $verbose and print STDERR "$0: No overlaps found\n";
  
  my $signal_peptide_overlap = &Bio::Pfam::PfamQC::family_overlaps_with_signal_peptide($family, $famObj, $pfamDB);
  if(defined($signal_peptide_overlap) and exists($signal_peptide_overlap->{total}) and $signal_peptide_overlap->{total}>0) {
    print "$0: There are $signal_peptide_overlap->{total} signal peptide overlaps, $signal_peptide_overlap->{seed} in SEED and $signal_peptide_overlap->{align} in ALIGN\n";
    exit(1);
  }

  #Check orcids 
  if (defined($famObj->{DESC}->{AU})) {
    foreach my $author (@{$famObj->DESC->AU}) { 
      my $author_entry;
      my @mismatches;
      # search for an author by ORCID...
      if ($author->{orcid}) {
        foreach my $entry ($pfamDB->getSchema->resultset('Author')->search({orcid => $author->{orcid}})) {
          if ($entry->author eq $author->{name}) {
            $author_entry = $entry;
          } else {
            push @mismatches, $entry->author;
          }   
        }   
      # ...or name
      } else {
        my @author_entry = $pfamDB->getSchema->resultset('Author')->search({author => $author->{name}});
        if(@author_entry > 1) {
          print "$0: There is more than one author with the name [".$author->{name}."] in the database. You must specify an orcid for ". $author->{name}." in the DESC file to distinguish it from the other author(s) with the same name.\n";
          exit(1);
        }   
        foreach my $au_entry (@author_entry) {  #There will only be one entry in @author_entry if we get to here 
          if($au_entry->orcid) {
            if($au_entry->orcid eq "NULL") {
              $author_entry=1;
              next;
            }
            print "[".$author->{name}. "] has an orcid in the database, but there is no orcid in your DESC file. Use 'addORCID.pl <family_dir>' to add the orcid to your DESC file.\n";
            exit(1);
          }   
          else {
            $author_entry = $au_entry;
          }   
        }   
      }   
      unless ($author_entry) {
        my $fail_msg .= qq(\nAuthor ") . $author->{name} . qq(" not found in database);
        $fail_msg .= (" or does not match ORCID " . $author->{orcid}) if $author->{orcid};
        $fail_msg .= "\n";
        if (@mismatches) {
          $fail_msg .= "Author with ORCID " . $author->{orcid} . " has names: " . join(", ", @mismatches) . "\n";
        }   
        $fail_msg .= "Please add new author using 'addORCID.pl <family_dir>'\n\n";
        print STDERR $fail_msg;
        exit(1);
      }   
    }   
  }
}

Bio::Pfam::PfamQC::checkDESCSpell( $family, $familyIO );


$verbose and  print STDERR "Looks like you sequences will be okay\n";

#pqc-check $family
unless ( Bio::Pfam::PfamQC::noFragsInSeed( $family, $famObj ) ) {
  exit(1);
}

$verbose and print STDERR "$0: Did not find any fragment in the SEED\n";

Bio::Pfam::PfamQC::nonRaggedSeed( $family, $famObj );

unless($isNew){
  unless(Bio::Pfam::PfamQC::noMissing($famObj, $svnFamObj, $family )){
    print STDERR "$0: It appears you are missing sequences compared to the SVN copy\n";  
  }
}
  


$verbose and print STDERR "$0: SEED does not appear to be ragged\n";


#Check all the references have been added
unless( Bio::Pfam::PfamQC::checkReferencesAdded($famObj)) {
    print STDERR "$0: Not all literature references in the CC lines have been added to the DESC file\n";
    exit(1);
}


sub help {

print<<EOF;

usage: $0 [options] <PFAM ACCESSION>

Performs all QC checks on the family:
Checks the format of the files for any issues;
Checks the sequences in the SEED and ALIGN files (as in 'pqc-seqs <PFAM ACCESSION>')
Checks the alignments for overlaps with filtering set (as in 'pqc-overlap-rdb -filter <PFAM ACCESSION>');
  The filter is set so that overlaps with a length of <20\% of the lowest scoring matching region length
  will be permitted (i.e. filtered) if they form <1\% of the regions in the ALIGN file.
  Filtering params are set on \$PFAM_CONFIG (overlap_rule field).
Checks the Seed has no fragments, is not ragged, and has no missing sequences;
Checks authors and references;

-help    : print this help message
-verbose : print extra statements as to what checks have been passed.

EOF
 

exit;
  
}
