#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;
use File::Copy;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::FamilyIO;

my $config = Bio::Pfam::Config->new;
my $familyIO = Bio::Pfam::FamilyIO->new;

my $pfamDB;
if ( $config->location eq 'WTSI' or $config->location eq 'EBI' ) {
      my $connect = $config->pfamlive;
      $pfamDB  = Bio::Pfam::PfamLiveDBManager->new( %{$connect} );
}else{
  die "This can only be performed at WTSI/EBI!\n";  
}

print STDOUT "This script will perform a whole series of steps designed to bring ".
             " back a family, which has inadvertantly been deleted!\n\n".
             "Please enter the accession (has to be accession) of the family".
             "that was deleted:";

my $accession;             
$_ = <STDIN>;
chomp;
if(/^(PF\d{5})$/){
  $accession = $1;    
}else{
  warn "No accession supplied\n";  
  exit;
}

print STDOUT "\n\nWhat was the last revision number of the SVN repository where the family was okay?"; 
$_ = <STDIN>;
chomp;
my $revision;
if(/^(\d{7})$/){
  $revision = $1;    
}else{
  warn "No revision number supplied, expectin a number greater than 1078000.\n";  
  exit;
}


#Connect to the database and see if we can find it in the dead_families table.
my $revDir = "Families.r".$revision;
system("svn co -r $revision --depth empty ".$config->svnRepos."/".$config->svnFamilies ." ".$revDir) and
  die "Failed to check out empty families directory, $revDir\n";

my $pwd = getcwd;
chdir($revDir) or die "Failed to chdir to $revDir\n";
#Now check out the specific family.
system("svn up -r $revision $accession") 
  and die "Failed to check out $accession:[$!]\n";

#Now check out the current working copy of the families
chdir($pwd);
system("svn co --depth empty ".$config->svnRepos."/".$config->svnFamilies ." Families") and
  die "Failed to check out families directory, current working copy\n";

chdir("Families") or die "Failed to chdir to Families dir\n";
mkdir($accession) or die "Failed to make directory $accession\n";
foreach my $f (@{ $config->mandatoryFamilyFiles }){
  copy($pwd."/".$revDir."/".$accession."/".$f, $pwd."/Families/".$accession."/".$f) 
    or die "Copy of $f failied:[$!]\n";
}


#Right, all of the files should now be in place. Now add them to subversion. Note
#this happens on the local copy and will not do anything to the repository until
#we commit into the database.
system("svn add $accession") and die "Failed to run svn add.";
my $whoami = getpwuid($>);

#Now read the family and build up our object.
my $famObj = $familyIO->loadPfamAFromLocalFile($accession, $pwd."/Families");

if($famObj->DESC->CL and $famObj->DESC->CL =~ /CL\d+/){
  print STDERR "Oh dear, this family was also part of clan....This script can not recover at the moment.\n";
  exit;  
}

#Now to fix the database, this needs to happen before the commit.
my $guard = $pfamDB->getSchema->txn_scope_guard;
#This will create the necessary row in the Pfam-A table.
$pfamDB->createPfamA($famObj, $pfamDB, $whoami);
#Delete the dead family data.
$pfamDB->getSchema->resultset("DeadFamilies")->search({pfama_acc => $accession})->delete;
$guard->commit;

#Commit back into the repository.  With this tag, it should trigger the pfci 
#process in the pre-commit hook and re-populate the database. 
system("svn commit -m \"PFCI:Resurecting familiy after it was mistakenly deleted.\" $accession")
  and die "Failed to commit family back in to the repository\n";

print STDERR "Finished:Please check the family!\n";
