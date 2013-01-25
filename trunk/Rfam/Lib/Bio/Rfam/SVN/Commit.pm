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

package Bio::Dfam::SVN::Commit;

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

use Bio::Dfam::Config;
use Bio::Dfam::FamilyIO;
use Bio::Dfam::LiveDBManager;
use Bio::Dfam::QC;
use Bio::Dfam::Classification;
use Data::Printer;

use base "SVN::Look";

#-------------------------------------------------------------------------------

=head1 METHODS
=cut

sub new {
  my ( $caller, $params ) = @_;

  my $class = ref($caller) || $caller;

  #call the constructor of the parent class, Person.
  my $self;
  if ( $params->{rev} ) {
    $self = $class->SUPER::new( $params->{repos}, -r => $params->{rev} );
  }
  elsif ( $params->{txn} ) {
    $self = $class->SUPER::new( $params->{repos}, -t => $params->{txn} );
  }

  $self->{config} = Bio::Dfam::Config->new;

  return bless( $self, $class );
}

sub commitEntry {
  my ( $self, $dfamDB ) = @_;

  #Make an object to respresent the family based on the SVN transcation
  my $familyIO = Bio::Dfam::FamilyIO->new;

  my ( $modelObj, $model, $dir );
  my @updated = $self->updated();

  #Need to put a transaction around this block
  my $guard = $dfamDB->getSchema->txn_scope_guard;

  if ( scalar(@updated) == 1 and $updated[0] eq 'DESC' ) {
    ( $modelObj, $model, $dir ) = $self->_getEntryObjFromTrans( $familyIO, 0 );
    #Perform QC on the family
    $self->_qualityControlEntry( $modelObj, $dir, $model, $dfamDB );
    $dfamDB->updateEntry($modelObj);

    if ( $modelObj->DESC->REFS ) {
      $dfamDB->updateLitRefs($modelObj);
    }
    if ( $modelObj->DESC->CLASS ) {
      $dfamDB->updateClassTags($modelObj);
    }
    if ( $modelObj->DESC->DBREFS ) {
      $dfamDB->updateDbXrefs($modelObj);
    }
  }
  else {
    ( $modelObj, $model, $dir ) = $self->_getEntryObjFromTrans( $familyIO, 0 );

    #Perform QC on the family
    $self->_qualityControlEntry( $modelObj, $dir, $model, $dfamDB );
    $dfamDB->updateEntry($modelObj);

    if ( $modelObj->DESC->REFS ) {
      $dfamDB->updateLitRefs($modelObj);
    }
    if ( $modelObj->DESC->CLASS ) {
      $dfamDB->updateClassTags($modelObj);
    }
    if ( $modelObj->DESC->DBREFS ) {
      $dfamDB->updateDbXrefs($modelObj);
    }
    $dfamDB->updateSeedReg($modelObj);
    $dfamDB->updateFullReg($modelObj);
    $dfamDB->updateRevReg($modelObj);
    $dfamDB->uploadHMMAndSEED( $modelObj, "$dir/$model", 0 );

  }

  #Need to intiate the view process.
  $dfamDB->intiatePostProcessJob( $modelObj, $self->author );
  $guard->commit;
}

sub commitNewEntry {
  my ( $self, $dfamDB ) = @_;

  #Make an object to respresent the family based on the SVN transcation
  my $familyIO = Bio::Dfam::FamilyIO->new;

  my $author = $self->author();

  my ( $newFamObj, $family, $dir ) =
    $self->_getEntryObjFromTrans( $familyIO, 1 );

  my $acc = $self->_assignAccession($dfamDB);

  $newFamObj->DESC->AC($acc);

  $self->_qualityControlEntry( $newFamObj, $dir, $family );

  #TODO  - put QC steps back in and sequence QC
  #Okay, if we get to here, then we should be okay!
  #Now upload the Entry to Dfam

  #Need to put a transaction around this block
  my $guard = $dfamDB->getSchema->txn_scope_guard;

  $dfamDB->createEntry( $newFamObj, $author );

  if ( $newFamObj->DESC->REFS ) {
    $dfamDB->updateLitRefs($newFamObj);
  }

  if ( $newFamObj->DESC->DBREFS ) {
    $dfamDB->updateDbXrefs($newFamObj);
  }

  if ( $newFamObj->DESC->CLASS ) {
    $dfamDB->updateClassTags($newFamObj);
  }

  if ( $newFamObj->DESC->MSP ) {
    $dfamDB->updateModelSpecificity($newFamObj);
  }

  $dfamDB->updateSeedReg($newFamObj);
  $dfamDB->updateFullReg($newFamObj);
  $dfamDB->updateRevReg($newFamObj);
  $dfamDB->uploadHMMAndSEED( $newFamObj, "$dir/$family", 1 );

  #Need to intiate the view process.
  $dfamDB->intiatePostProcessJob( $newFamObj, $self->author );

  $guard->commit;
}

sub _assignAccession {
  my ( $self, $dfamDB ) = @_;

  my @allAccessions;
  my $allModelData     = $dfamDB->getAllModelData;
  my $allDeadModelData = $dfamDB->getAllDeadModelData;
  unless ($allModelData)     { $allModelData     = []; }
  unless ($allDeadModelData) { $allDeadModelData = []; }
  foreach my $fam ( @{$allModelData}, @{$allDeadModelData} ) {
    my ($tmpAcc) = $fam->accession =~ /(\d+)/;
    push( @allAccessions, $tmpAcc );
  }

  push( @allAccessions, 0 );
  my @allAccSorted = sort { $a <=> $b } @allAccessions;
  my $nextAccNo = $allAccSorted[$#allAccSorted];
  $nextAccNo++;

  if ( length($nextAccNo) > 7 ) {
    die "Accession length exceeded\n";
  }

  #Now put the number in the correct format.
  my $acc = "DF" . "0" x ( 7 - length($nextAccNo) ) . $nextAccNo;

  return ($acc);

}

sub commitClassification {
  my ( $self, $dfamDB ) = @_;

  my $classObj = Bio::Dfam::Classification->new( $self->{config} );
  $self->_getClassObjFromTrans($classObj);

  $dfamDB->updateClassification($classObj);
  $dfamDB->updateWikipedia($classObj);
}

sub initiateModelView {
  my ( $self, $dfamDB ) = @_;

  #Make an object to respresent the family based on the SVN transcation
  my $familyIO = Bio::Pfam::FamilyIO->new;
  my ( $famObj, $family, $dir ) = $self->_getFamilyObjFromTrans( $familyIO, 0 );

  #If this family is part of a clan, we need to compete it
  if ( $famObj->DESC->CL and $famObj->DESC->CL =~ /\CL\d+/ ) {
    Bio::Pfam::ViewProcess::initiateClanViewProcess( $famObj->DESC->CL,
      $self->author, $self->{config} );
  }
  else {

    #If we have not died, then we should be good to go!
    Bio::Pfam::ViewProcess::initiateViewProcess( $famObj, $self->author,
      $self->{config} );
  }
}

sub _getEntryObjFromTrans {
  my ( $self, $familyIO, $isNew ) = @_;

  #Are we dealing with a new family, set path accordingly.
  my $svnPath =
    ( $isNew == 1 )
    ? $self->{config}->svnNewModels
    : $self->{config}->svnModels;

  #At this point we have no idea of the name of the family.
  my @updated_files = $self->updated();
  my @added_files   = $self->added();
  my @deleted_files = $self->deleted();
  unless ( scalar(@updated_files)
    or scalar(@added_files)
    or scalar(@deleted_files) )
  {
    confess("Trying to commit a family with no updated/added/deleted files\n");
  }

  #Determine where the path is!
  my ( $path, $family );
  foreach my $f ( @updated_files, @added_files, @deleted_files ) {
    if ( $f =~ m|($svnPath/)(\S+)(/\w+)| ) {
      $path   = "$1$2";
      $family = $2;
      last;
    }
    elsif ( $f =~ m|($svnPath/)(\S+)(/)| ) {
      $path   = "$1$2";
      $family = "$2";
    }
  }

  print STDERR p(@updated_files);
  print STDERR p(@added_files);
  print STDERR p(@deleted_files);

  unless ( $path and $family ) {
    confess(
      "Failed to find path [$path] to family [$family] in SVN repository\n");
  }

  #Write all of the files for this transaction to disk and then read them
  my $dir = File::Temp->newdir();
  mkdir("$dir/$family") or confess("Could not make $dir/$family:[$!]");
  my $params;
  foreach my $f ( @{ $self->{config}->mandatoryFiles } ) {
    print STDERR "Catting $f\n";
    my $fh;
    my @file = $self->cat("$path/$f");
    open( $fh, ">$dir/$family/$f" ) or die "Could not open $dir/$f";
    foreach (@file) {
      print $fh "$_\n";
    }
    close($fh);
  }

  #Fix timestamps.....
  foreach my $file (
    qw(SEED HMM OUTPUT DFAMOUT OUPUT.rev DFAMOUT.rev scores ALIGN DESC))
  {
    next unless ( -e "$dir/$family/$file" );

    #Fudge the access time and modification times
    my ( $atime, $mtime );
    $atime = $mtime = time;
    utime $atime, $mtime, "$dir/$family/$file";
  }

  my $famObj = $familyIO->loadDfamFromLocalFile( $family, $dir, 'svn' );

  return ( $famObj, $family, $dir );
}

sub _getClassObjFromTrans {
  my ( $self, $class ) = @_;

  #Are we dealing with a new family, set path accordingly.
  my $svnPath = $self->{config}->svnClassification;

  #At this point we have no idea of the name of the family.
  my @updated_files = $self->updated();

  unless ( scalar(@updated_files) ) {
    confess("Trying to commit a classifiation with no updated files\n");
  }

  print STDERR p(@updated_files);

#unless($path and $family){
#  confess("Failed to find path [$path] to classification [$c] in SVN repository\n");
#}

  #Write all of the files for this transaction to disk and then read them
  my $dir = File::Temp->newdir();
  my $params;
  my $f = shift(@updated_files);
  my $fh;
  my @file = $self->cat("$f");

  #
  open( $fh, "+>", "$dir/classification" )
    or die "Could not open $dir/classification";
  foreach (@file) {
    print $fh "$_\n";
  }
  close($fh);
  open( $fh, "<", "$dir/classification" );

  #seek($fh, 0, 1);

  $class->parseClassificationFile($fh);
  close($fh);
  print STDERR p( $class->classification );
}

sub _qualityControlEntry {
  my ( $self, $modelObj, $dir, $model ) = @_;
  print STDERR "In  _qualityControlEntry, $modelObj\n";

  #Perform all of the format checks
  unless ( Bio::Dfam::QC::passesAllFormatChecks( $modelObj, "$dir/$model" ) ) {
    exit(1);
  }
}

sub moveFamily {
  my ( $self, $pfamDB ) = @_;

  #At this point we have no idea of the name of the family.
  #Make sure that there are not files deleted or added,
  #and that the DESC file is the only file modifies.

  my @updated_files = $self->updated();
  foreach my $f (@updated_files) {
    if ( $f !~ m|(.*/Families/\S+/DESC)$| ) {
      confess(
"Trying to move a family with updated files (other than the DESC file)\n"
      );
    }
  }

  my @deleted_files = $self->deleted();
  if ( scalar(@deleted_files) ) {
    confess("Trying to move a family with deleted files\n");
  }

  my @added_files = $self->added();
  if ( scalar(@added_files) ) {
    confess("Trying to move a family with added files\n");
  }

  my $familyIO = Bio::Pfam::FamilyIO->new;
  my ( $famObj, $family, $dir ) = $self->_getFamilyObjFromTrans( $familyIO, 0 );
  $familyIO->movePfamAInRDB( $famObj, $pfamDB );

  #If we have not died, then we should be good to go!
  Bio::Pfam::ViewProcess::initiateViewProcess( $famObj, $self->author,
    $self->{config} );
}

sub deleteEntry {
  my ( $self, $dfamDB, $comment, $forward ) = @_;

  my ($family);
  my @deleted_files = $self->deleted();
  my $svnPath       = $self->{config}->svnModels;
  foreach my $f (@deleted_files) {
    if ( $f =~ m|($svnPath/)(\S+)(/)| ) {
      $family = "$2";
    }
  }

  unless ($family) {
    confess(
      "Failed to find which family in to be removed from the SVN repository\n");
  }

  my $familyIO = Bio::Dfam::FamilyIO->new;
  my $author   = $self->author();
  $familyIO->deleteEntryInRDB( $family, $dfamDB, $comment, $forward, $author );

}

sub allowCommit {
  my ( $self, $dfamDB ) = @_;

  my @lock_data =
    $dfamDB->getSchema->resultset('Lock')->search( { 'locked' => 1 } );

  if (@lock_data) {
    my $lock_data = shift @lock_data;    #Should only ever be one row
    return $lock_data;
  }
  else {
    return 0;
  }
}

=head1 COPYRIGHT

File: Commit.pm

Author: Rob Finn (finnr@janelia.hhmi.org)

Copyright (c) 2010, Howard Hughes Medical Institute, All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, 
   this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice, 
   this list of conditions and the following disclaimer in the documentation 
   and/or other materials provided with the distribution.
   3. Neither the name of the Howard Hughes Medical Institute nor the names of 
   its contributors may be used to endorse or promote products derived from 
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, ANY 
IMPLIED WARRANTIES OF MERCHANTABILITY, NON-INFRINGEMENT, OR FITNESS FOR A 
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; REASONABLE ROYALTIES; 
OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER 
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING 
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
SUCH DAMAGE. 

=cut

1;
