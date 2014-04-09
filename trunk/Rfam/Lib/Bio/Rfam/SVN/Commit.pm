=head1 NAME

MODULENAME - a module that 

=cut

package Bio::Rfam::SVN::Commit;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: Commit.pm 

Copyright (c) 2013: 


Author: Rob Finn (rdf 'at' ebi.ac.uk or finnr 'at' janelia.hhmi.org)
Incept: finnr, Jan 29, 2013 10:33:00 PM

=cut

#-------------------------------------------------------------------------------

use strict;
use warnings;
use File::Temp;
use Carp;

use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::QC;
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
#Todo - pass in the config object if we have already made it.
  $self->{config} = Bio::Rfam::Config->new;

  return bless( $self, $class );
}

sub commitEntry {
  my ( $self ) = @_;

  #Make an object to respresent the family based on the SVN transcation
  my $familyIO = Bio::Rfam::FamilyIO->new;

  my ( $familyObj, $family, $dir );
 
  ( $familyObj, $family, $dir ) = $self->_getEntryObjFromTrans( $familyIO, 0 );

  $self->_commitEntry($familyObj);

}

sub _commitEntry {
  my ($self, $familyObj, $isNew) = @_;
  
  my $rfamdb = $self->{config}->rfamlive;
  #Need to put a transaction around this block
  my $guard = $rfamdb->txn_scope_guard;  
  #Regardless of the way that we have come in to this part,
  #update the database with the DESC file information.
   my @updated = $self->updated(); 
  my @pages = keys(%{$familyObj->DESC->WIKI});
  my $row = $rfamdb->resultset('Wikitext')->find_or_create({'title' => $pages[0]});
  if($isNew){
    $rfamdb->resultset('Family')->createFamilyFromObj($familyObj, $row->auto_wiki);
  }else {
    $rfamdb->resultset('Family')->updateFamilyFromObj($familyObj, $row->auto_wiki);
  }
  $rfamdb->resultset('LiteratureReference')->find_or_createFromFamilyObj( $familyObj );
  $rfamdb->resultset('DatabaseLink')->find_or_createFromFamilyObj( $familyObj );  
 


  if ( scalar(@updated) == 1 and $updated[0] eq 'DESC' ) {
  }else{
    #There is more to change than just the DESC file....
    $rfamdb->resultset('SeedRegion')->updateSeedRegionsFromFamilyObj( $familyObj );
    $rfamdb->resultset('FullRegion')->updateFullRegionsFromFamilyObj( $familyObj );
    $rfamdb->resultset('FamilyFile')->uploadFilesFromFamilyObj( $familyObj );
  }
  
  #Need to intiate the view process.
  $rfamdb->resultset('PostProcess')->initiatePostProcessJob( $familyObj, $self->author );
  #Finish the transaction.
  $guard->commit;
}

sub commitNewEntry {
  my ( $self ) = @_;

  #Make an object to respresent the family based on the SVN transcation
  my $familyIO = Bio::Rfam::FamilyIO->new;

  #Determin who is adding this entry
  my $author = $self->author();

  my ( $newFamObj, $family, $dir ) =
    $self->_getEntryObjFromTrans( $familyIO, 1 );

  my $acc = $self->_assignAccession();

  $newFamObj->DESC->AC($acc);

  #TODO  - put QC steps back in and sequence QC
  #$self->_qualityControlEntry( $newFamObj, $dir, $family );

  #Okay, if we get to here, then we should be okay!
  #Now upload the Entry to Rfam
  $self->_commitEntry($newFamObj, 1);
}

sub _assignAccession {
  my ( $self ) = @_;

  my @allAccessions;
  my $rfamdb = $self->{config}->rfamlive;
  
  my $allFamilyAccs = $rfamdb->resultset('Family')->getAllFamilyAcc;
  my $allDeadFamilyAccs = $rfamdb->resultset('DeadFamily')->getAllDeadFamilyAcc;
    
  foreach my $fam ( @{$allFamilyAccs}, @{$allDeadFamilyAccs} ) {
    my ($tmpAcc) = $fam->rfam_acc =~ /(\d+)/;
    push( @allAccessions, $tmpAcc );
  }

  #Need this when the database is new, and there are no entries.
  push( @allAccessions, 0 );
  my @allAccSorted = sort { $a <=> $b } @allAccessions;
  my $nextAccNo = $allAccSorted[$#allAccSorted];
  $nextAccNo++;

  if ( length($nextAccNo) > 5 ) {
    die "Accession length exceeded\n";
  }

  #Now put the number in the correct format.
  my $acc = "RF" . "0" x ( 5 - length($nextAccNo) ) . $nextAccNo;

  return ($acc);
}

sub _getEntryObjFromTrans {
  my ( $self, $familyIO, $isNew ) = @_;

  #Are we dealing with a new family, set path accordingly.
  my $svnPath =
    ( $isNew == 1 )
    ? $self->{config}->svnNewFamilies
    : $self->{config}->svnFamilies;

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
    if ( $f =~ m|($svnPath/)(\S+)(\/\w+)| ) {
      $path   = "$1$2";
      $family = $2;
      last;
    }
    elsif ( $f =~ m|($svnPath/)(\S+)(\/)| ) {
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

  my $famObj = $familyIO->loadRfamFromLocalFile( $family, $dir, 'svn' );

  return ( $famObj, $family, $dir );
}

sub _qualityControlEntry {
  my ( $self, $modelObj, $dir, $model ) = @_;
  print STDERR "In  _qualityControlEntry, $modelObj\n";

  #Perform all of the format checks
  unless ( Bio::Rfam::QC::passesAllFormatChecks( $modelObj, "$dir/$model" ) ) {
    exit(1);
  }
}

#TODO - rewrite
sub moveFamily {
  my ( $self ) = @_;

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
  my ( $familyObj, $family, $dir ) = $self->_getFamilyObjFromTrans( $familyIO, 0 );
  $self->_commitEntry($familyObj);
}

sub deleteFamily {
  my ( $self, $comment, $forward, $author ) = @_;

  #Check we got a comment
  unless ($comment) {
    confess('Did not get a comment as to why this family is being killed');
  }

  #Get the database connection.
  my $rfamdb = $self->{config}->rfamlive;
  #Need to put a transaction around this block
  my $guard = $rfamdb->txn_scope_guard;
  
  #Determine the family from the transaction
  my ($family);
  my @deleted_files = $self->deleted();
  my $svnPath       = $self->{config}->svnFamilies;
  foreach my $f (@deleted_files) {
    #Need to see if there is a / on $svnPath;
    if($svnPath !~ m|\S+/|){
      $svnPath .= '/';
    }
    
    if ( $f =~ m|($svnPath)(\S+)(\/)| ) {
      $family = "$2";
    }
  }

  unless ($family) {
    confess(
      "Failed to find which family is to be removed from the SVN repository\n");
  }


  unless ( $family and $family =~ /RF\d{5}/ ) {
    confess("Did not get a Rfam accession\n");
  }

  my $entry = $rfamdb->resultset('Family')->find({'rfam_acc' => $family});
  my $user = $self->author;
  #Now make the dead family entry!
  $entry->delete();
  #$rfamdb->resultset('Family')->delete('rfam_acc' => $family);
  $rfamdb->resultset('DeadFamily')->createFromFamilyRow($entry, $comment, $forward, $user);

  #Finish the transaction.
  $guard->commit;
}

sub allowCommit {
  my ( $self  ) = @_;
  
  my $rfamDB = $self->{config}->rfamlive;
  my @lock_data = 
    $rfamDB->resultset('Lock')->search( { 'locked' => 1 } );

  if (@lock_data) {
    my $lock_data = shift @lock_data;    #Should only ever be one row
    return $lock_data;
  } else {
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
