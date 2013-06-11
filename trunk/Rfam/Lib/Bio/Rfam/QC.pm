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

1;
