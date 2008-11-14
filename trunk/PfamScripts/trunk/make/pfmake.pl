#!/usr/local/bin/perl

use strict;
use warnings;
use Getopt::Long;

use Bio::Pfam::Config;
use Bio::Pfam::HMM::HMMResultsIO;
use Bio::Pfam::SeqFetch;


main( @ARGV ) unless caller(  );


sub main {
  my( $seqThrs, $domThrs, $evalue, $seqDB, $help);
#-----------------------------------------------------------------------------------------
#Read options
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
  
  if($evalue and ($seqThrs or $domThrs)){
    die "You have specified an evalue ($evalue) and bits score\n"; 
  }
  
  #Check that the bits thresholds make sense
  if(!$evalue){
    if($domThrs and !$seqThrs){
      #Use the domain threshold for the sequence threshold
      $seqThrs = $domThrs; 
    }elsif($seqThrs and !$domThrs){
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
    $domThrs = $HMMResults->domain_bits_cutoff_from_evalue($evalue);
    $seqThrs = $domThrs if($domThrs); 
  }
  
#-----------------------------------------------------------------------------------------
# Read edits, apply the edits to the HMMResults, write the scores file   
  my %edits;
  my($oldSeqThrs, $oldDomThrs) = &readDESC(\%edits);
  
  $HMMResults->applyEdits( \%edits ) if(keys %edits);  
  
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
  $HMMResults->write_scores_file(\*SCORES);
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

  open(TEMP,">DESC.temp") or die "Could not open DESC.temp. But made align... [$!]";
  open(DESC,"DESC")       or die "Could not open DESC. But made align...[$!]\n";
  my($tcSeq, $tcUnit) = $HMMResults->lowestTrue;
  my($ncSeq, $ncUnit)  = $HMMResults->highestNoise;
  
  while(<DESC>) {
    if(/^GA/) {
      print TEMP sprintf("GA   %.2f %.2f;\n", $HMMResults->seqThr, $HMMResults->domThr);
    }elsif(/^NC/){
      print TEMP sprintf("NC   %.2f %.2f;\n", $ncSeq, $ncUnit);
    }elsif(/^TC/){
      print TEMP sprintf("TC   %.2f %.2f;\n", $tcSeq, $tcUnit);
    }else{ 
      print TEMP $_;
    }
  }
  close(TEMP);
  
  rename("DESC","DESC.old") or die "Failed to rename DESC to DESC.old:[$!]\n";
  rename("DESC.temp","DESC") or die "Failed to rename DESC.temp to DESC:[$!]\n";;
}

# SUBROUTINES ----------------------------------------------------------------------------


sub help {
print<<EOF;

    $0 - builts a Pfam alignment from HMMER3 HMM/search files. If no thresholds are set, they are obtained
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


sub readDESC {
  my ( $editsRef ) = shift;
  my ( $oldseqthr, $olddomthr);
  #Grab any edits and GA thresholds out of the DESC file;
  if( -s "DESC" ) {
    open( DESC, "DESC" ) or die "Could not open a DESC file to read thresholds $!";
    while(<DESC>) {
      if(/^GA\s+(\S+)\s+(\S+);/){
        $oldseqthr = $1;
        $olddomthr = $2;
  	  }elsif(/^ED\s+(\S+)\/(\d+)-(\d+);\s+(\S+)\/(\d+)-(\d+);\s*$/) {
  	    chomp;
  	    if( $1 ne $4 ) {
  		    die "ED line [$_] has mismatched protein ids\n";
  	      push( @{ $editsRef->{ $1 } }, { 'line'     => $_,
  				                           'oldstart' => $2,
  				                           'oldend'   => $3,
  				                           'newstart' => $5,
  				                           'newend'   => $6 } );
        }
  	  }elsif(/^ED\s+(\S+)\/(\d+)-(\d+);\s*$/) {
  	    push( @{ $editsRef->{ $1 } }, { 'line'     => $_,
  				       'oldstart' => $2,
  				       'oldend'   => $3 } );
  	   }
    }
    close(DESC);
    return ($oldseqthr, $olddomthr)
  }
}

#-----------------------------------------------------------------------------------------
#hack until we get H3 hmmalign
sub makeALIGN {
  my ($config, $fasta) = @_;  
  
  unless(-s $fasta){
    die "The fasta file has zero size:[$!]\n"; 
  }
  
  system($config->hmmer2bin."/hmmbuild -F HMM2 SEED > /dev/null") and die "Could not run H2 hmmbuild:[$!]";
  system($config->hmmer2bin."/hmmalign -q HMM2 fa > ALIGN.sto") and die "Could not run H2 hmmalign:[$!]";
  unless(-s "ALIGN"){
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

sub writeDESC {
  
  
}