#!/usr/local/bin/perl
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
use Bio::Pfam::PfamQC;

#-------------------------------------------------------------------------------
# Deal with all of the options 

my ($message, $ignore, $onlydesc, $iterated, $addToClan, $removeFromClan, $help);


&GetOptions( "m=s"              => \$message,
             "i"                => \$ignore,
             "onlydesc"         => \$onlydesc,
             "iterated"         => \$iterated,
             "add_to_clan"      => \$addToClan,
             "remove_from_clan" => \$removeFromClan,
             "help"             => \$help );

my $family = shift;
chomp($family);

unless($family){
  warn "\n***** No family passed  *****\n\n"; 
  help();
}

if(@ARGV){
  warn "\n***** $0 no longer supports multiple family check-ins *****\n\n";
  help();
}

if($iterated){
  warn "\n***** -iterated not current supported *****\n\n";
  help();  
}

if($removeFromClan and $addToClan) {
  warn "\n***** You cannot use the -add_family and -remove_family options together *****\n\n";
}

help() if($help);

#-------------------------------------------------------------------------------
my $pwd = getcwd;

if( !(-d "$pwd/$family") ) {
    die "$0: [$pwd/$family] is not a current directory.\nMust be in the parent directory of the family to check in\n";
}

if( !-w "$pwd/$family" ) {
    die "$0: I can't write to directory [$pwd/$family].  Check the permissions.\n";
}

if(!(-d "$pwd/$family/.svn") ){
  die "$0: [$pwd/$family] does not look like that it is from the Pfam subversion repository";
}

#-------------------------------------------------------------------------------
# If a message is supplied, then write it to file such that the code reference
# that deals with the SVN log message can grab it.

if(-s ".defaultpfci"){
  unlink(".defaultpfci") or die "Could not remove old default check-in message\n"; 
}


if($message){
  open(M, ">.defaultpfci") or die "Could not open message file\n";  
  print M $message;
  close(M);
}


#-------------------------------------------------------------------------------
#Initial SVN stuff

#Check that family exists in svn
my $client = Bio::Pfam::SVN::Client->new;
$client->checkFamilyExists($family);

#Need to check that the family is up-to-date 


#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware 

my $familyIO = Bio::Pfam::FamilyIO->new;

if($onlydesc){
  $client->addPFANNLog();
  #AC
  
  #my $upFamObj = $familyIO->loadPfamADESCFromLocalFile($family, $pwd);
  print STDERR "Successfully loaded $family thorugh middleware\n";
  
  #Check the desc accessions are the same
  #if($upFamObj->DESC->AC ne $oldFamObj->DESC->AC){
  #  die "Accession error, your local copy does not match the repository\n";  
  #}  
  
}else{
  $client->addPFCILog();
  if( !Bio::Pfam::PfamQC::checkFamilyFiles( $family) ){
    print "pfci: $family contains errors.  You should rebuild this family.\n";
    exit(1);
  }
  my $upFamObj = $familyIO->loadPfamAFromLocalFile($family, $pwd);
  print STDERR "Successfully loaded $family thorugh middleware\n";
  
  my $oldFamObj = $familyIO->loadPfamAFromSVN($family, $client);
  print STDERR "Successfully loaded remote $family thorugh middleware\n";

  #AC present
  if($upFamObj->DESC->AC ne $oldFamObj->DESC->AC){
    die "Accession error, your local copy does not match the repository\n";  
  }


  #These are more sanity checks
  unless($ignore){
    unless(Bio::Pfam::PfamQC::sequenceChecker($upFamObj)){
      print "pfci: $family contains errors.  You should rebuild this family.\n";
      exit(1);
    }
  
  
    unless(Bio::Pfam::PfamQC::noMissing($upFamObj, $oldFamObj, $family )){
      exit(1);   
    }

    #pqc-check $family
  
    unless(Bio::Pfam::PfamQC::noFragsInSeed($upFamObj, $family)){
      exit(1);   
    }
  
  
    unless(Bio::Pfam::PfamQC::nonRaggedSeed($upFamObj)){
      exit; 
    }
  }
  #NEED TO CHECK THAT ASSURTIONS COVER ALL FORMAT CHECKS.....
  
  
  unless(Bio::Pfam::PfamQC::passesAllFormatChecks($upFamObj, $family)){
    exit(1); 
  }
  exit;
  #If we are at sanger, perform this prior to commit 
  

}

#-------------------------------------------------------------------------------
#If we get here, then great! We can now check the family in!
my $caught_cntrl_c; 
$SIG{INT} = sub {$caught_cntrl_c = 1;};   # don't allow control C for a bit!

$client->commitFamily($family); 
 
#Remove any file containing the check-in message
if(-s ".defaultpfci"){
  unlink(".defaultpfci") or die "Could not remove old default check-in message\n";
}

#
if( $caught_cntrl_c ) {
  print STDERR "\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** but this could be very bad.  You really must tell someone about this!\n";
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
   if($addToClan){
      
  }

  if($removeFromClan){
  
  } 
}