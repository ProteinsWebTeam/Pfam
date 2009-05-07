#!/usr/local/bin/perl

use strict;
use warnings;
use Getopt::Long;
#use Time::Elapse;


use Bio::Pfam::Config;
use Bio::Pfam::HMM::HMMResultsIO;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::SeqFetch;


main( @ARGV ) unless caller(  );


sub main {
  my( $seqThrs, $domThrs, $evalue, $seqDB, $help);
#-----------------------------------------------------------------------------------------
#Read options

  Getopt::Long::Configure('no_ignore_case');
  
  GetOptions (
    "e=s" => \$evalue,
    "T=s" => \$seqThrs,
    "t=s" => \$domThrs,
    "d=s" => \$seqDB,
    "h"   => \$help,
  );

  if($help){
    &help; 
  }
  
  if($evalue and (defined($seqThrs) or defined($domThrs))){
    die "You have specified an evalue ($evalue) and bits score\n"; 
  }

  #Check that the bits thresholds make sense
  if(!$evalue){
    if(defined($domThrs) and ! defined($seqThrs)){
      #Use the domain threshold for the sequence threshold
      $seqThrs = $domThrs; 
    }elsif(defined $seqThrs and ! defined $domThrs){
      die "You cannot define a sequence threshold without defining the domain threshold!";
    }  
  }
  
  if(@ARGV){
    warn "Additional paramters parsed into $0: ".join(" ", @ARGV)." - these have been ignored\n"; 
  }
 
#-----------------------------------------------------------------------------------------
# Check that the sequence database is present
  my $config = Bio::Pfam::Config->new;
  
  unless($seqDB){
    $seqDB = $config->pfamseqLoc."/pfamseq"; 
  }
  die "Could not find $seqDB\n" unless(-s $seqDB);   
  
#-----------------------------------------------------------------------------------------
#Read in the PFAMOUT put file and convert evalue to bits (if defined)

  #Need make sure that we have a PFAMOUT file
  unless(-s "PFAMOUT"){
    die "There is no PFAMOUT file:[$!]\n"; 
  }
  
  #Now read in the PFAMOUT file
  my $HMMResultsIO = Bio::Pfam::HMM::HMMResultsIO->new;
  my $HMMResults = $HMMResultsIO->parsePFAMOUT( "PFAMOUT" );
  
  #If an evalue has been specified, lets try and estimate a bits score
  if($evalue){
    $domThrs = $HMMResults->domainBitsCutoffFromEvalue($evalue);
    $seqThrs = $domThrs if($domThrs); 
  }
  
#-----------------------------------------------------------------------------------------
# Read edits, apply the edits to the HMMResults, write the scores file   
  my $descObj;
  my $io = Bio::Pfam::FamilyIO->new;
  if( -s "DESC" ) {
    $descObj = $io->parseDESC("DESC");
  }else{
    die "Current we need to have to have a DESC\n";  
  }
  my $oldSeqThrs = $descObj->CUTGA->{seq};
  my $oldDomThrs = $descObj->CUTGA->{dom};
  
  
  
  
  $HMMResults->applyEdits( $descObj->EDITS ) if( $descObj->EDITS );  
  
  #Set the thresholds on the results set.  
  if($domThrs and $seqThrs){
    $HMMResults->domThr($domThrs);
    $HMMResults->seqThr($seqThrs);
  }elsif(defined($oldSeqThrs) and defined($oldDomThrs)){
    $HMMResults->domThr($oldDomThrs);
    $HMMResults->seqThr($oldSeqThrs);
  }else{
    warn "No thresholds specified, using default of 25.0 bits\n";
    $HMMResults->seqThr( 25.0 );
    $HMMResults->domThr( 25.0 );
  }
  
  #Apply the threshold when writing the scores file
  open(SCORES, ">scores") or die "Could not open file scores:[$!]\n";
  $HMMResultsIO->writeScoresFile($HMMResults);
  close(SCORES);
  
#-----------------------------------------------------------------------------------------
#Fetch all of the significant sequences   
  my $fasta = "FA";
  open(SC, "scores") || die "Error opening scores: [$!]\n";
  open(FA,">$fasta") || die "Error opening fasta file $fasta: [$!]\n";
  
  #This is actually a bit of a waste!  We could just use the HMM results set
  my %seqList;
  my $noSeqsRequested = Bio::Pfam::SeqFetch::parseScores(\*SC, \%seqList);
  close (SC) || die "Error closing scores file:[$!]\n";  
  my $noSeqsFound = &Bio::Pfam::SeqFetch::fetchSeqs(\%seqList, $seqDB, \*FA);
  close(FA) || die "Error closing fasta file:[$!]\n";  
  
  if($noSeqsFound != $noSeqsRequested){
    die "Did not find all the sequences you requested: Requested $noSeqsRequested; Got $noSeqsFound\n";
  }  
  
  makeALIGN($config, $fasta);              
  # Remove files that allow clan to be unmade.  If family thresholds
  # have been changed or ED lines added this can lead to ALIGN and DESC
  # getting out of sync.  THis was an issue during overlap resolution.
  if(-s "scores_before_clan"){
    unlink("scores_before_clan") or die "Could not unlink scores_before_clan:[$!]\n";
  }
  if(-s "ALIGN_before_clan"){
   unlink("ALIGN_before_clan") or die "Could not unlink ALIGN_before_clan\n";
  }
#-----------------------------------------------------------------------------------------
#Finally, put the new threshold in the DESC files  
  

  my($tcSeq, $tcUnit) = $HMMResults->lowestTrue;
  my($ncSeq, $ncUnit)  = $HMMResults->highestNoise;
  
  $descObj->CUTGA( { seq => $HMMResults->seqThr, dom => $HMMResults->domThr} );
  $descObj->CUTNC( { seq => $ncSeq, dom => $ncUnit });
  $descObj->CUTTC( { seq => $tcSeq, dom => $tcUnit });
  
  rename("DESC","DESC.old") or die "Failed to rename DESC to DESC.old:[$!]\n";
  $io->writeDESC( $descObj );
  
}

# SUBROUTINES ----------------------------------------------------------------------------


sub help {
print<<EOF;

    $0 - builds a Pfam alignment from HMMER3 HMM/search files. If no thresholds are set, they are obtained
    from the DESC file.  If none are set, a default 25.0 bits will be used.

    Usage: $0 <options>

      -T             Sequence threshold - you must specifiy a domain threshold as well.
      -t             Domain threshold
      -e             Evalue based Domain threshold
      -d             set the full path to pfamseq, e.g. /data/blastdb/pfamseq
      -h             prints this help

EOF
    exit(1);  
}


#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
#hack until we get H3 hmmalign
sub makeALIGN {
  my ($config, $fasta) = @_;  
  
  unless(-s $fasta){
    die "The fasta file has zero size:[$!]\n"; 
  }

  system($config->hmmer3bin."/hmmalign HMM FA > ALIGN.sto") and die "Could not run H3 hmmalign:[$!]";
  unless(-s "ALIGN.sto"){
    die "Tried to make alignment, but it does not seem to be there:[$!]\n";
  }
  
  #Move sideways and remove stockholm format
  #We like nice un blocked alignments!
  my %reformat;
  my $maxlength = 0;
  open(ALISTO, "ALIGN.sto") || die "Could not open ALIGN.sto\n";
  open(ALIGN, ">ALIGN") || die "Could not open final align file:[$!]\n";
  
  while(<ALISTO>){
    if(/^(\S+)\s+(\S+)$/){
      $maxlength = length($1) if ($maxlength < length($1));
      $reformat{$1} .= $2;
    }elsif(/^#/){
      next;
    }elsif(/\/\//){
      next;
    }elsif(/^$/){
      next; 
    }else{ 
      warn "Did not parse $_ from the stockholm format\n";
    }
  }
  close(ALISTO) || die "Error closing ALIGN.sto\n";
  
  $maxlength += 2;
  foreach my $nse (keys %reformat){
     print ALIGN sprintf("%-".$maxlength."s", $nse);
     print ALIGN $reformat{$nse}."\n";
  }
  close(ALIGN);
  
  if( !-s "ALIGN" ) {
      warn "ALIGN file has zero size - this is probably not good\n";
  }
}

