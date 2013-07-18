=head1 NAME

Bio::Rfam::QC - a module that performs QC on Rfam data.

=cut

package Bio::Rfam::QC;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: QC.pm 

Copyright (c) 2013: 


Author: Rob Finn (rdf 'at' ebi.ac.uk or finnr 'at' janelia.hhmi.org)
Incept: finnr, Jan 25, 2013 8:43:56 AM

=cut

use strict;
use warnings;
use File::Temp qw(tempfile);
use File::Copy;
#-------------------------------------------------------------------------------

=head1 METHODS

=cut

#Templating this off the old code......
sub checkFamilFiles {
  my ( $family, $upFamilyObj) = @_;  

  &checkQCPerformed;
  &checkTimeStamps();

  
}


sub checkQCPerformed {
my ($dir, $acc);
    #need to add in this for checking the qc has been done-dont allow ci otherwise.
    if (! -e "$dir/$acc/qcpassed") {
    die "rfci: [$acc] has not been passed by qc checks so not ready to check in - run rqc-all.pl\n";
    }
#check qc.passed is older than the relevant files
    
 if( (-M "$dir/$acc/DESC"     < -M "$dir/$acc/qcpassed") ||
     (-M "$dir/$acc/CM"        < -M "$dir/$acc/qcpassed") ||
     (-M "$dir/$acc/SEED"     < -M "$dir/$acc/qcpassed") ||
     (-M "$dir/$acc/OUTPUT"   < -M "$dir/$acc/qcpassed") ||
     (-M "$dir/$acc/out.list" < -M "$dir/$acc/qcpassed")
    ) {
        die "You need to rerun the rqc-all.pl as at least one of DESC/SEED/OUTPUT/out.list has changed since you ran it last\n";
 }
}

sub checkCM {
  my ($familyObj) = @_;
  
  my $error = 0;
  #Check that the CM and internal HMM agree.
  if($familyObj->CM->cmHeader->{nSeq} != $familyObj->CM->hmmHeader->{nSeq}){
    $error = 1;
    print STDERR "Somehow the number of sequences in the CM does not agree with its own internal HMM\n";
    return $error;
  }
  
# Cross reference the number of SEED sequences with the number in the CM.
  if($familyObj->CM->cmHeader->{nSeq} != $familyObj->SEED->nseq){
    $error = 1;
    print STDERR "The number of sequences in the CM does not agree with the number in the SEED.\n";
    return $error;
  }
  my $acc = $familyObj->DESC->AC;            
  print STDERR  "SEED:CM:$acc: No serious format errors found\n";
  return $error;
}

sub checkSeedSeqsFound {

#Works of accessions only.

#No need for double loop, Hash the SEED sequences, Then loop over the full.
# Talk to Eric - he may have a better idea.

# 

}


sub compareOldAndNewFull {
#....

}

sub checkTimestamps {


}

sub checkDescCumplosryFields {


}

sub spellCheckFreeText {


}


sub valid_sequences {


}

#
#Coding - rpc-seqs_coding.pl
#Get Eric to look at rpc-ss-cons.pl (does this work?)

#------------------------------------------------------------------------------
=head2 

  Title    : checkSpell
  Incept   : finnr, Jul 17, 2013 1:29:27 PM
  Usage    : Bio::Rfam::QC::checkSpell($dir, $dictPath, $familyIOObj)
  Function : This takes the DESC file and grabs out the free text fields, removes
           : our tags and writes a temporary file. It then runs ispell over the
           : contents of the file, interactively with the users. Finally, the 
           : data is written back into the file.
  Args     : Path containting the family, Path to dictionary file used by ispell,
           : a familyIO object (optional)
  Returns  : An error flag if encountered.
  
=cut

sub checkSpell {
  my ($fam, $dictionary, $familyIO) = @_;
  
  my $error = 0;
  #
  unless ($familyIO) {
    $familyIO = Bio::Pfam::FamilyIO->new;
  }

  #Make sure that the DESC file is vaild to start off with
  eval{
    $familyIO->parseDESC("$fam/DESC");
  };
  if($@){
    print STDERR $@;
    $error = 1
  }
  
  my (%line);
  my ($lineNo) = 0;

  open( DESC, "$fam/DESC" )
    || die "Can't open DESC file for family $fam:[$!]\n";
  
  while (<DESC>) {
    # If a free text line add to %lines
    if (/^RT   (\.*)$/) {
      $line{$lineNo} = $1;
    }
    elsif (/^CC   (.*)$/) {
      $line{$lineNo} = $1;
    }
    elsif (/^RC   (.*)$/) {
      $line{$lineNo} = $1;
    }
    elsif (/^DC   (.*)$/) {
      $line{$lineNo} = $1;
    }
    elsif (/^DE   (.*)$/) {
      $line{$lineNo} = $1;
    }
    $lineNo++;
  }
  close(DESC);

  my @lineNos = sort { $a <=> $b; } keys %line;
  
  my ( $bit, @line_number_array );

  #Now make temporary file and write free text
  my ($tfh, $tfilename) = tempfile();
  
  foreach ( @lineNos ) {
    #Print all free text lines.
    print $tfh $line{ $_ }, "\n";
  }
  close $tfh;

  # Start ispell session on file
  system("ispell -W 0 -w 0123456789 -p$dictionary $tfilename");
  
  # Now need to put changes back into DESC file
  my ( %editedline, $line_number );
  open( TMP, '<', $tfilename ) || die "Can't open temp file $tfilename:[$!]\n";
  
  while (<TMP>) {
    if (/^(.*)$/) {
      $line_number = shift @lineNos;
      $editedline{"$line_number"} = $1;
    }
    else {
      die "unrecognised line [$_]\n Serious error!\n";
    }
  }
  close(TMP);

  # Write out new DESC file
  open( TEMPDESC, ">$fam/DESC.$$" )
    || die "Can't write to temp DESC file for family $fam\n";

  open( DESC, "$fam/DESC" )
    || die "Can't open DESC file for fam $fam\n";

  my ($prefix);
  $lineNo = 0;

  while (<DESC>) {
    if ( $editedline{$lineNo} ) {

      # Find if DE, RT or CC line
      if ( $_ =~ /^(\S+)/ ) {
        $prefix = $1;
      }
      else {
        die "unrecognised line [$_]\n";
      }

      # Write out line
      print TEMPDESC "$prefix   $editedline{$lineNo}\n";
    }
    else {
      print TEMPDESC;
    }
    $lineNo++;
  }
  close(DESC);
  close(TEMPDESC);

  # Move DESC across
  copy( "$fam/DESC.$$", "$fam/DESC" );
  # Clean up
  unlink("$fam/DESC.$$");

  #Now make sure that I have not screwed anyting up!  It is possible that the
  #line lengths could overflow.
  $familyIO->parseDESC("$fam/DESC");
  
  return ($error);
}

=head2 

  Title    : ssStats
  Incept   : EPN, Thu Jul 18 15:34:22 2013
  Usage    : Bio::Rfam::QC::checkSpell($dir, $dictPath, $familyIOObj)
  Function : Calculates and outputs per-family, per-sequence and per-basepair
           : statistics to 'ss-stats-perfamily', 'ss-stats-persequence' and 
           : 'ss-stats-perbasepair' files.
  Args     : A Family object.
  Returns  : void
  
=cut

sub ssStats {
  my ( $famObj ) = @_;

  my $seed = $famObj->SEED;
  if($seed->any_allgap_columns) { die "ERROR, SEED has all gap columns"; }
  if($seed->nseq < 2)           { die "ERROR, SEED has less than 2 sequences"; }
  $seed->set_name($famObj->DESC->ID);
  $seed->set_accession($famObj->DESC->AC);
  $seed->weight_GSC();
  $seed->rfam_qc_stats("ss-stats-perfamily", "ss-stats-persequence", "ss-stats-perbasepair");
}




1;
