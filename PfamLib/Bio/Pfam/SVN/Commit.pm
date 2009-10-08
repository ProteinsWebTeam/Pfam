#
#Commit.pm
#
# Author:        rdf
# Maintainer:    $Id: Commit.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $
# Version:       $Revision: 1.1 $
# Created:       Dec 2, 2008
# Last Modified: $Date: 2009-10-08 12:27:28 $
=head1 NAME

Template - a short description of the class

=cut

package Bio::Pfam::SVN::Commit;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: Commit.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $

=head1 COPYRIGHT

File: Commit.pm

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

 This is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 or see the on-line version at http://www.gnu.org/copyleft/gpl.txt
 
=cut

use strict;
use warnings;
use File::Temp;
use Carp;

use Bio::Pfam::Config;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::PfamQC;
use Bio::Pfam::ViewProcess;
use Bio::Pfam::ClanIO;
use Bio::Pfam::Clan::Compete;

use base "SVN::Look";

#-------------------------------------------------------------------------------

=head1 METHODS
=cut


sub new {
  my($caller, $params) = @_;
  
  my $class = ref($caller) || $caller;
  
  #call the constructor of the parent class, Person.
  my $self;
  if($params->{rev}){
    $self = $class->SUPER::new( $params->{repos}, -r => $params->{rev} );
  }elsif($params->{txn}){
    $self = $class->SUPER::new( $params->{repos}, -t => $params->{txn} );
  }
  
  $self->{config} = Bio::Pfam::Config->new;
  
  return bless($self, $class);  
}

sub commitFamily {
  my ( $self, $pfamDB ) = @_;
  
  #Make an object to respresent the family based on the SVN transcation
  my $familyIO = Bio::Pfam::FamilyIO->new;
  my ($famObj, $family, $dir) = $self->_getFamilyObjFromTrans($familyIO, 0);
  
  #Perform QC on the family
  $self->_qualityControlFamily($famObj, $dir, $family, $pfamDB);
  
  #Okay, if we get to here, then we should be okay!
  #Now upload the family to Pfam  
  $familyIO->updatePfamAInRDB($famObj, $pfamDB, 0);
  $familyIO->uploadPfamAHMM($famObj, $pfamDB, $dir, 0);
  $familyIO->uploadPfamAAligns($famObj, $pfamDB, $dir, 0);
  
  #If this family is part of a clan, we need to compete it
  if($famObj->DESC->CL and $famObj->DESC->CL =~ /\CL\d+/){
    Bio::Pfam::ViewProcess::initiateClanViewProcess($famObj->DESC->CL, $self->author, $self->{config});
  }else{
    #If we have not died, then we should be good to go! 
    Bio::Pfam::ViewProcess::initiateViewProcess($famObj, $self->author, $self->{config});
  }
}

sub commitNewFamily {
  my ( $self, $pfamDB ) = @_;
 
  #Make an object to respresent the family based on the SVN transcation
  my $familyIO = Bio::Pfam::FamilyIO->new;
  my ($famObj, $family, $dir) = $self->_getFamilyObjFromTrans($familyIO, 1);  
  
  #Assign the accession.
  my $acc = $self->_assignAccession($pfamDB);
  
  $famObj->DESC->AC($acc);
  #Now update the desc file
  
#TODO
  
  #Now perform the QC steps.....
  $self->_qualityControlFamily($famObj, $dir, $family, $pfamDB);
  #Need to check the sequences.....
#TODO 
  
  #Okay, if we get to here, then we should be okay!
  #Now upload the family to Pfam  
  my $author = $self->author();
  $familyIO->updatePfamAInRDB($famObj, $pfamDB, 1, $author);
  $familyIO->uploadPfamAHMM($famObj, $pfamDB, $dir, 1);
  $familyIO->uploadPfamAAligns($famObj, $pfamDB, $dir, 1);  
  
  #If this family is part of a clan, we need to compete it
  if($famObj->DESC->CL and $famObj->DESC->CL =~ /\CL\d+/){
    Bio::Pfam::ViewProcess::initiateClanViewProcess($famObj->DESC->CL, $self->author, $self->{config});
  }else{
    #If we have not died, then we should be good to go! 
    Bio::Pfam::ViewProcess::initiateViewProcess($famObj, $self->author, $self->{config});
  }
}

sub initiateFamilyView {
  my ( $self, $pfamDB ) = @_;
  
  #Make an object to respresent the family based on the SVN transcation
  my $familyIO = Bio::Pfam::FamilyIO->new;
  my ($famObj, $family, $dir) = $self->_getFamilyObjFromTrans($familyIO, 0);
  
  #If this family is part of a clan, we need to compete it
  if($famObj->DESC->CL and $famObj->DESC->CL =~ /\CL\d+/){
    Bio::Pfam::ViewProcess::initiateClanViewProcess($famObj->DESC->CL, $self->author, $self->{config});
  }else{
    #If we have not died, then we should be good to go! 
    Bio::Pfam::ViewProcess::initiateViewProcess($famObj, $self->author, $self->{config});
  }
}

sub commitNewClan {
  my ( $self, $pfamDB ) = @_;
 
  #Make an object to respresent the family based on the SVN transcation
  my $clanIO = Bio::Pfam::ClanIO->new;
  #
  my ($clanObj, $clan, $dir) = $self->_getClanObjFromTrans($clanIO, 1);  
  
  #Assign the accession.
  #
  my $acc = $self->_assignClanAccession($pfamDB);
  
  $clanObj->DESC->AC($acc);  
  
  #Okay, if we get to here, then we should be okay!
  #Now upload the family to Pfam  
  my $author = $self->author();
  $clanIO->updateClanInRDB($clanObj, $pfamDB, 1, $author );
  
}

sub commitClan {
  my ( $self, $pfamDB ) = @_;
 
  #Make an object to respresent the family based on the SVN transcation
  my $clanIO = Bio::Pfam::ClanIO->new;
  my ($clanObj, $clan, $dir) = $self->_getClanObjFromTrans($clanIO, 0);  
   
  #Okay, if we get to here, then we should be okay!
  #Now upload the clan to the database
  
  $clanIO->updateClanInRDB($clanObj, $pfamDB, 0);
  
}

sub _getFamilyObjFromTrans {
  my ($self, $familyIO, $isNew) = @_;
  
  #Are we dealing with a new family, set path accordingly.
  my $svnPath = ( $isNew == 1 ) ? $self->{config}->svnNewFamilies : $self->{config}->svnFamilies;
  
  #At this point we have no idea of the name of the family.
  my @updated_files = $self->updated();
  my @added_files = $self->added();
  my @deleted_files = $self->deleted();
  unless(scalar(@updated_files) or scalar(@added_files) or scalar(@deleted_files)){
    confess("Trying to commit a family with no updated/added/deleted files\n");  
  }
  
  #Determine where the path is!
  my ($path, $family);
  foreach my $f (@updated_files, @added_files, @deleted_files) {
    if( $f =~ m|($svnPath/)(\S+)(/\w+)| ){
      $path = "$1$2";
      $family = $2;
      last;
    }elsif( $f =~ m|($svnPath/)(\S+)(/)| ){
      $path = "$1$2";
      $family = "$2";
    }
  }
  
  unless($path and $family){
    confess("Failed to find path to family in SVN repository\n");
  }
  
  
  #Write all of the files for this transaction to disk and then read them
  my $dir =  File::Temp->newdir();
  mkdir("$dir/$family") or confess("Could not make $dir/$family:[$!]");
  my $params;
  foreach  my $f ( @{ $self->{config}->mandatoryFamilyFiles }) {
    print STDERR "Catting $f\n";
    my $fh;
    my @file =  $self->cat("$path/$f");
    open( $fh, ">$dir/$family/$f") or die "Could not open $dir/$f";
    foreach (@file){
      print $fh "$_\n";
    }
    close($fh);
  }
 
  my $famObj = $familyIO->loadPfamAFromLocalFile($family, $dir, 'svn');
  
  return($famObj, $family, $dir);
}


sub _getClanObjFromTrans {
  my ($self, $clanIO, $isNew) = @_;
  
  #Are we dealing with a new clan, set path accordingly.
  my $svnPath = ( $isNew == 1 ) ? $self->{config}->svnNewClans : $self->{config}->svnClans;
  
  #At this point we have no idea of the name of the clan.
  my @updated_files = $self->updated();
  my @added_files = $self->added();
  unless(scalar(@updated_files) or scalar(@added_files)){
    confess("Trying to check-in a clan with no updated/added files\n");  
  }
  
  #Determine where the path is!
  my ($path, $clan);
  foreach my $f (@updated_files, @added_files) {
    if( $f =~ m|($svnPath/)(\S+)(/\w+)| ){
      $path = "$1$2";
      $clan = $2;
      last;
    }
  }
  
  unless($path and $clan){
    confess("Failed to find path to family in SVN repository\n");
  }
  
  
  #Write all of the files for this transaction to disk and then read them
  my $dir =  File::Temp->newdir();
  mkdir("$dir/$clan") or confess("Could not make $dir/$clan:[$!]");
  my $params;
#TODO - Change this to use config 
  foreach  my $f ( qw(CLANDESC) ) {
    my $fh;
    my @file =  $self->cat("$path/$f");
    open( $fh, ">$dir/$clan/$f") or die "Could not open $dir/$clan/$f";
    foreach (@file){
      print $fh "$_\n";
    }
    close($fh);
  }
 
  my $clanObj = $clanIO->loadClanFromLocalFile($clan, $dir, 'svn');
  
  return($clanObj, $clan, $dir);
}

sub _qualityControlFamily {
  my  ($self, $famObj, $dir, $family, $pfamDB) = @_; 
  
 #Perform all of the format checks
  unless(Bio::Pfam::PfamQC::passesAllFormatChecks($famObj, "$dir/$family" )){
    exit(1); 
  }
  
  #Need to add sequence checker here!
  
  #Now apply any edits......
  if($famObj->DESC->EDITS){
    print STDERR "Applying Edits\n";
    $famObj->PFAMOUT->applyEdits( $famObj->DESC->EDITS ) if( $famObj->DESC->EDITS ); 
  }
  
  #Now find out what this family is _allowed_ to overlap with
  my %ignore; 
  
  #See if this family is part of a clan!
  if($famObj->DESC->CL){
    
    my %ignore;
    
    unless($famObj->DESC->CL =~ /CL\d{4}/){
      confess("Got a clan cross reference, but accession is in the wrong format\n");  
    }
    #Now check that this family is part of the clan
    my $clanIO = Bio::Pfam::ClanIO->new;
    my $clan = $clanIO->loadClanFromRDB($famObj->DESC->CL, $pfamDB);
    
    unless($clan and $clan->DESC->AC eq $famObj->DESC->CL){
      confess("Failed to get a clan object that represents ".$famObj->DESC->CL."\n");
    }
    
    #unless($clan->familyIsMember($family)){
    #  confess("$family is not part of the clan ".$famObj->DESC->CL." \n");   
    #}
    
    if($clan->DESC->MEMB){
      foreach my $m (@{ $clan->DESC->MEMB }){
        $ignore{$m}++;  
        print STDERR "Ignoring $m in overlaps\n";
      }
    }
      
  }else{
    #Looks like we have not got a CL, check that it is not being removed from the 
    #clan
     
  }  
  
  
  my $overlaps = Bio::Pfam::PfamQC::family_overlaps_with_db( $family,\%ignore , undef, $pfamDB, $famObj );
  warn "$family: found $overlaps overlaps\n";
  if ($overlaps) {
    confess("Found overlaps\n");
  } 
}

sub moveClan {
  my ( $self, $pfamDB ) = @_;
  
  #At this point we have no idea of the name of the family.
  #Make sure that there are not files deleted or added,
  #and that the DESC file is the only file modifies.

  my @updated_files = $self->updated();
  foreach my $f (@updated_files){
    if( $f !~ m|(.*/Clans/\S+/CLANDESC)$|){
      confess("Trying to move a clan with updated files (other than the CLANDESC file)\n"); 
    }
  }
  
  my @deleted_files = $self->deleted();
  if( scalar(@deleted_files) ){
    confess("Trying to move a clan with deleted files\n"); 
  } 
  
  my @added_files = $self->added();
  if( scalar(@added_files) ){
    confess("Trying to move a clan with added files\n"); 
  } 
  
  my $clanIO = Bio::Pfam::ClanIO->new;
  my ($clanObj, $clan, $dir) = $self->_getClanObjFromTrans($clanIO, 0);
  $clanIO->moveClanInRDB($clanObj, $pfamDB);
  
  #If we have not died, then we should be good to go! 
  Bio::Pfam::ViewProcess::initiateClanViewProcess($clan, $self->author, $self->{config});
}



sub moveFamily {
  my ( $self, $pfamDB ) = @_;
  
  #At this point we have no idea of the name of the family.
  #Make sure that there are not files deleted or added,
  #and that the DESC file is the only file modifies.

  my @updated_files = $self->updated();
  foreach my $f (@updated_files){
    if( $f !~ m|(.*/Families/\S+/DESC)$|){
      confess("Trying to move a family with updated files (other than the DESC file)\n"); 
    }
  }
  
  my @deleted_files = $self->deleted();
  if( scalar(@deleted_files) ){
    confess("Trying to move a family with deleted files\n"); 
  } 
  
  my @added_files = $self->added();
  if( scalar(@added_files) ){
    confess("Trying to move a family with added files\n"); 
  } 
  
  my $familyIO = Bio::Pfam::FamilyIO->new;
  my ($famObj, $family, $dir) = $self->_getFamilyObjFromTrans($familyIO, 0);
  $familyIO->movePfamAInRDB($famObj, $pfamDB);
  
  #If we have not died, then we should be good to go! 
  Bio::Pfam::ViewProcess::initiateViewProcess($famObj, $self->author, $self->{config});
}

sub deleteFamily {
  my ( $self, $pfamDB, $comment, $forward ) = @_;
  
  my ($family);
  my @deleted_files = $self->deleted();
  my $svnPath =  $self->{config}->svnFamilies;
  foreach my $f (@deleted_files){
   if( $f =~ m|($svnPath/)(\S+)(/)| ){
      $family = "$2"; 
    }
  }
  
  unless($family){
    confess("Failed to find which family in to be removed from the SVN repository\n");
  }
  
  my $familyIO = Bio::Pfam::FamilyIO->new;
  my $author = $self->author();
  $familyIO->deletePfamAInRDB($family, $pfamDB, $comment, $forward, $author);
  
}


sub deleteClan {
  my ( $self, $pfamDB, $comment, $forward ) = @_;
  
  my ($clan);
  my @deleted_files = $self->deleted();
  my $svnPath =  $self->{config}->svnClans;
  foreach my $f (@deleted_files){
   if( $f =~ m|($svnPath/)(\S+)(/)| ){
      $clan = "$2"; 
    }
  }
  
  unless($clan){
    confess("Failed to find which family in to be removed from the SVN repository\n");
  }
  
  my $clanIO = Bio::Pfam::ClanIO->new;
  my $author = $self->author();
  $clanIO->deleteClanInRDB($clan, $pfamDB, $comment, $forward, $author);
}

sub _assignAccession {
  my ($self, $pfamDB) = @_;  
  
  my @allAccessions;
  
  foreach my $fam (@{ $pfamDB->getAllPfamFamilyData }, @{ $pfamDB->getAllDeadFamilyData }){
    my ($tmpAcc) = $fam->pfama_acc =~ /PF(\d+)/;
    push (@allAccessions, $tmpAcc);
  }
  
  my @allAccSorted = sort{ $a <=> $b } @allAccessions;
  my $nextAccNo = $allAccSorted[$#allAccSorted];
  $nextAccNo++;
  
  if(length($nextAccNo) > 5){
    die "Accession length exceeded\n"; 
  }
  
  #Now put the number in the correct format.
  my $acc = "PF". "0" x (5 - length($nextAccNo)).$nextAccNo;
  
  return($acc);
  
}

sub _assignClanAccession {
  my ($self, $pfamDB) = @_;  
  
  my @allAccessions;
  
  foreach my $clan (@{ $pfamDB->getAllClanData }, @{ $pfamDB->getAllDeadClanData }){
    my ($tmpAcc) = $clan->clan_acc =~ /CL(\d+)/;
    push (@allAccessions, $tmpAcc);
  }
  
  my @allAccSorted = sort{ $a <=> $b } @allAccessions;
  my $nextAccNo = $allAccSorted[$#allAccSorted];
  $nextAccNo++;
  
  if(length($nextAccNo) > 4){
    die "Accession length exceeded\n"; 
  }
  
  #Now put the number in the correct format.
  my $acc = "CL". "0" x (4 - length($nextAccNo)).$nextAccNo;
  
  return($acc);
}

sub allowCommit {
    my ($self, $pfamDB) = @_;

    my @lock_data = $pfamDB->getSchema
	->resultset('Lock')->search({ 'locked' => 1});

    if(@lock_data) {
	my $lock_data = shift @lock_data; #Should only ever be one row
        return $lock_data;
    }
    else {
	return 0;
    }

}


  
#This should go in the Clan IO Module!  
sub updateClanMembership{
  my($self, $pfamDB, $clan, $pfam) = @_;
  my $pfamRow = $pfamDB->getPfamData($pfam);
  #Should check at this point to makes sure the family is not already part of another clan.
  
  
  my $clanRow = $pfamDB->getClanData($clan);
  
  $pfamDB->updateClanMembership($clanRow->auto_clan, $pfamRow->auto_pfama);
}

sub removeFamilyFromClanMembership{
  my($self, $pfamDB, $clan, $pfam) = @_;
  my $pfamRow = $pfamDB->getPfamData($pfam);
  my $clanRow = $pfamDB->getClanData($clan);
  
  $pfamDB->removeFamilyFromClanMembership($clanRow->auto_clan, $pfamRow->auto_pfama);
}


#  #$self->familyName($family);
#  #$self->pathToFamily($path);
#  #my @added_files   = $txnlook->added();
##  #my @updated_files = $txnlook->updated();
#  #my @deleted_files = $txnlook->deleted();
#  #my @changed_files = $txnlook->changed();
#  
#  #my $dir = "/tmp"; #=
#  
#  #Write all of the files for this transaction to disk and then read them
#  my $dir =  File::Temp->newdir();
#  mkdir("$dir/$family") or confess("Could not make $dir/$family:[$!]");
#  my $params;
#  foreach  my $f ( @{ $self->{config}->mandatoryFamilyFiles }) {
#    my $fh;
#    my @file =  $self->cat("$path/$f");
#    open( $fh, ">$dir/$family/$f") or die "Could not open $dir/$f";
#    foreach (@file){
#      print $fh "$_\n";
#    }
#    close($fh);
#  }
#  
#  my $familyIO = Bio::Pfam::FamilyIO->new;
#  my $famObj = $familyIO->loadPfamAFromLocalFile($family, $dir);
#  
#  $famObj->DESC->ID($family);
#  
#  #Perform all of the format checks
#  #unless(Bio::Pfam::PfamQC::passesAllFormatChecks($famObj, "$dir/$family")){
#  #  exit(1); 
#  #}
#  
#  
#  #Now find out what this family is _allowed_ to overlap with
#  push (my @ignore, $family);
#  
#  
#  
#  my $overlaps = Bio::Pfam::PfamQC::family_overlaps_with_db( "$dir/$family", \@ignore, undef, $pfamDB );
#  warn "$family: found $overlaps overlaps\n";
#  #if ($overlaps) {
#  #  confess("Found overlaps\n");
#  #}else {
#  #  exit(0);
#  #}
#  
#  #Okay, if we get to here, then we should be okay!
#  #Now upload the family to Pfam  
#  $familyIO->updatePfamAInRDB($famObj, $pfamDB);
#  $familyIO->uploadPfamAHMM($famObj, $pfamDB, $dir);
#  $familyIO->uploadPfamAAligns($famObj, $pfamDB, $dir);
#  #If we have not died, then we should be good to go! 
#  Bio::Pfam::ViewProcess::initiateViewProcess($famObj, $self->{config});


#### DETRITUS

#$self->familyName($family);
  #$self->pathToFamily($path);
  #my @added_files   = $txnlook->added();
#  #my @updated_files = $txnlook->updated();
  #my @deleted_files = $txnlook->deleted();
  #my @changed_files = $txnlook->changed();
  
  #my $dir = "/tmp"; #=

1;
