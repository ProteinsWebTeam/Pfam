#!/usr/bin/env perl

# motif_scan.pl - 	RNA Motif Family Scan - Searches a database of motif CMs (made using cmpress) 
#                       against Rfam family SEEDs and populates the Rfam database with the results 
#                       linking families to motifs and vice-versa.

#################################################################################
# Set modules/packages to use
use warnings;
use strict;
use Cwd;
use Getopt::Long;
use IO::File;
use Data::Printer;
use Carp;
use Data::Dumper;

use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::Family::MSA;
use Bio::Rfam::Infernal;
use Bio::Rfam::Utils;
use Bio::Rfam::MotifMatch;

use Bio::Easel::MSA;
use Bio::Easel::SqFile;
use Bio::Easel::Random;

#################################################################################
# Set the default values that may be changed via command line options
my $fractionMotifs      = 0.1;      # The fraction of motifs in a stockholm required 
my $minNumberHits       = 2;        # Minimum number of matches between a motif and a family required
my $idf                 = 0.9;      # Filtering value for esl-weight
my $help;
my $config = Bio::Rfam::Config->new;    # Set the location of programs and general config

# Get the options from the command line
GetOptions(  	"fm=s"          => \$fractionMotifs,
	     	"min=s"         => \$minNumberHits,
	     	"idf=s"         => \$idf,
	     	"h"             => \$help,)
           || die "ERROR, unrecognized option\n";	

# Get the rfam accession
my $rfam_acc = shift;

# Determine if help has been specified as command line option
if ( $help ) {
  &help();
  exit(1);
}

# Set the user
my $user = getpwuid($<);
if (! defined $user || length($user) == 0) {
  die "FATAL: failed to run get login or getpwuid($<)]!\n[$!]";
}

# Specify the CMdb directory - CHANGE TO BE SET IN RFAM_CONFIG
my $CMdb = "CM";
my $CM_dir="/nfs/production/xfam/rfam/MOTIFS/cmdb/";
my $CMdb_loc=$CM_dir.$CMdb;

# Determine if the CMdb file specified as arguments exists
if(not -e $CMdb_loc) {
  print "MISSING the essential CM file or Family Accession!\n";
  exit(1);
  }
  
# Load the family object from the database
my $famIO=Bio::Rfam::FamilyIO->new;
my $familyObj=$famIO->loadRfamFromRDB($rfam_acc);
unless (defined $familyObj) {
  print "Family not in the database! \n";
  exit;
}

# Generate the SEED file in stockholm format
my $famSeedObj=$familyObj->SEED;

File::Path::make_path("/nfs/production/xfam/rfam/MOTIFS/results/$rfam_acc/");
my $fam_dir="/nfs/production/xfam/rfam/MOTIFS/results/$rfam_acc/";

my $STK = $fam_dir."SEED";
$famSeedObj->write_msa($STK, "stockholm");
 
# Perform filtering on the SEED and generate the weights/statistics
my ($filteredSTK, $weights, $sum)=stockholm2filteredSTK($STK,$idf,$fam_dir);
my $num_seq = stockholmSeqStats($filteredSTK);

#------------------------------------------------------------------------------------
# Create a hash and an array containing each motif
my %Motif_hash;
my @all_motifs;
my $rfamdb = $config->rfamlive;
my @completeMotResultSet = $rfamdb->resultset('Motif')->all();

foreach my $mot (@completeMotResultSet) {
  my $motif_acc = $mot->get_column('motif_acc');
  $Motif_hash{$motif_acc} = {
    NUM_HITS       => 0,      # Number of hits
    FREQ_HITS      => 0,      # Frequency of hits
    SUM_BITS       => 0,      # Sum of bit scores 
    W_SUM_BITS     => 0,      # Sum of weighted bit scores
    MOTIF_ID       => $mot->get_column('motif_id')
  };
  push(@all_motifs, $motif_acc);
}

#-----------------------------------------------------------------------------------
# Run cmscan with filtered SEED files and get sequence stats/weights of the filtered SEED.
print "Running Round 1 of cmscan: Filtered SEED \n";
submitToCmscan($rfam_acc, $filteredSTK, $CMdb_loc, $fam_dir, "filtered.TBL");

# Parse the results of the first cmscan to an array of match objects with only matches
my $TBL = "/nfs/production/xfam/rfam/MOTIFS/results/$rfam_acc/filtered.TBL";
my @motifMatches = parseTBL2MotifMatchObj($TBL,$rfam_acc, @all_motifs);

#------------------------------------------------------------------------------------
# Perform Calculations from results of the first cmscan 

foreach my $motifMatchObj (@motifMatches) {

  # Count the number of times a motif matches a family
  my $TBL_motif_acc = $motifMatchObj->MOTIF_ACC;
  $Motif_hash{$TBL_motif_acc}->{NUM_HITS}+=1;
  
  # Calculate the sum of the bit scores
  $Motif_hash{$TBL_motif_acc}->{SUM_BITS}+=sprintf("%.3f",$motifMatchObj->BIT_SCORE);

  # Calculate the weighted bit scores
  my $seq_id = $motifMatchObj->RFAMSEQ_ACC."/".$motifMatchObj->RFAMSEQ_START."-".$motifMatchObj->RFAMSEQ_STOP;
  my $seq_weight;
  if (defined $weights->{$seq_id}) {
    $seq_weight = $weights->{$seq_id};
  }
  else { $seq_weight = 1.0 }
  if ($sum == 0) { $sum = 1.0} 
  my $w_sum_of_bits=sprintf("%.3f",(($seq_weight*$motifMatchObj->BIT_SCORE)/$sum));
  $Motif_hash{$TBL_motif_acc}->{W_SUM_BITS}+=sprintf("%.3f",$w_sum_of_bits);
}

# Calculate the frequency of sequences that match each motif
foreach my $motif (keys %Motif_hash) {
 my $num_hits = ($Motif_hash{$motif}->{NUM_HITS});
 my $freq=$num_hits/$num_seq;
 $Motif_hash{$motif}->{FREQ_HITS}=sprintf("%.3f",$freq);
}

#-------------------------------------------------------------------------------------
# Create an outlist statistics table of the matches for the filtered SEED match calculations
my $outlist = $fam_dir."outlist";
open (my $fh, '>', $outlist) or die "Could not open file $outlist $!";

# Print the header line of outlist
printf $fh (      "%-12s %-12s %12s %15s %20s %25s\n", 
                  "Rfam Acc", 
                  "Motif Acc", 
                  "Number Hits", 
                  "Fraction Hits", 
                  "Sum bits", 
                  "Average Weighted bits");

# Print a line in the outlist for each motif to family match
foreach my $motif (keys %Motif_hash) {
  if ($Motif_hash{$motif}->{NUM_HITS} > 1) {
    printf $fh (  "%-12s %-12s %12s %15s %20s %25s\n", 
                  $rfam_acc, $motif, 
                  $Motif_hash{$motif}->{NUM_HITS}, 
                  $Motif_hash{$motif}->{FREQ_HITS}, 
                  $Motif_hash{$motif}->{SUM_BITS}, 
                  $Motif_hash{$motif}->{W_SUM_BITS});
  }
}
close $fh;

#--------------------------------------------------------------------------------------
# Create a list of motif accessions which fufill the requirments and can be used as
# matches. Theses motifs will be used to annoate the SEED.
my @allowed_motifs;
foreach my $motif (keys %Motif_hash) {
 if ($Motif_hash{$motif}->{NUM_HITS} >= $minNumberHits) { 
   if ($Motif_hash{$motif}->{FREQ_HITS} >= $fractionMotifs) {
     push (@allowed_motifs, $motif);
   }
 }
}

#-------------------------------------------------------------------------------------
# Run cmscan on the unfiltered SEED and create a TBL only accepting motifs that have meet the filtered criterea.
print "Running Round 2 of cmscan: Non-filtered SEED \n";
submitToCmscan($rfam_acc, $STK, $CMdb_loc, $fam_dir, "TBL");


# Parse the results of the first cmscan to an array of match objects with only matches
$TBL = "/nfs/production/xfam/rfam/MOTIFS/results/$rfam_acc/TBL";
my @AllowedMatches = parseTBL2MotifMatchObj($TBL,$rfam_acc, @allowed_motifs);

foreach my $motifMatchObj (@AllowedMatches) {
 # Put a guard around the transaction
  my $guard = $rfamdb->txn_scope_guard;

 # Insert the values into the MotifMatch table
  $rfamdb->resultset('MotifMatch')->find_or_createFromMotifMatchObj( $motifMatchObj );

 # Close transaction
  $guard->commit;
}

#-----------------------------------------------------------------------------------
# Assign motif labels to each motif. Labels are subsiquently used in the markup

my %motifLabels;
my %taken;
foreach my $motif (keys %Motif_hash) {
  my $label_motif_id = $Motif_hash{$motif}->{MOTIF_ID};
  $motifLabels{$motif} = assign_motif_label($label_motif_id,\%taken) if not defined $motifLabels{$label_motif_id};
  $taken{$motifLabels{$motif}}=1;
}

my $motifLabels=\%motifLabels;

#-----------------------------------------------------------------------------------
# Create an array of hashes to be used in the stockholm markup / overlap check
      
my %f2;
foreach my $motifMatchObj (@AllowedMatches) {
  my $motifMatchSeqId = $motifMatchObj->RFAMSEQ_ACC."/".$motifMatchObj->RFAMSEQ_START."-".$motifMatchObj->RFAMSEQ_STOP;
  
  my $seqid       = $motifMatchSeqId;
  my $start       = $motifMatchObj->QUERY_START;
  my $end         = $motifMatchObj->QUERY_STOP;
  my $strand      = 1;
  my $score       = $motifMatchObj->BIT_SCORE;
  my $evalue      = $motifMatchObj->E_VALUE;
  my $f_motif_acc = $motifMatchObj->MOTIF_ACC;
  my $label       = $motifLabels{$f_motif_acc};

  $strand = 1;
  if( $end < $start ) {
    ( $start, $end ) = ( $end, $start );
    $strand = -1;
  }

  my %f = (  seqid           => $seqid,
             start           => $start,
             end             => $end,
             strand          => $strand,
             score           => $score,
             evalue          => $evalue,
             motif_acc       => $f_motif_acc,
             label           => $label );

  push( @{ $f2{$seqid} }, \%f );
}

#------------------------------------------------------------------------------------
# Mark up the original SEED file with the accepted motifs.
# We add a #GF line below the matching sequence and add letters which
# correspond to the motif under the sequence.

# Compute the length of the alignment

print "Marking up alignment\n";
my $alnLength = compute_length_of_aligment($STK);
open(F, "esl-reformat -ru --mingap pfam $STK | ") or die "FATAL: could not open pipe for reading $STK\n[$!]"; 
my @stk = <F>;
my %positions2seqid; 
my %seqid2positions; 
my %motifLines; #hash of arrays: seq1:MT.1,MT.2,... seq2:MT.1,MT.2,...
my %motiffedSeqLineNumbers;
my $cnt=0;
my $firstSeqLine;
my $features=\%f2;

foreach my $stk (@stk){
  if($stk=~/^(\S+)\s+\S+$/){
    $positions2seqid{$cnt}=$1;
    $seqid2positions{$1}=$cnt;
    $firstSeqLine=$cnt if not defined $firstSeqLine;
  }
  $cnt++;
}

# Create a motif markup line for each sequence with an annoation taking care of overlaps
my @seqCoords2alnCoords;
foreach my $seqid (keys %seqid2positions) {
  if(defined $features->{$seqid}){   
    my $fpos=0;
    foreach my $f ( @{$features->{$seqid}} ){
      my $alnSeq=$stk[$seqid2positions{$seqid}];
        if($alnSeq=~/^(\S+)\s+(\S+)$/){
          my ($lid,$lseq)=($1,$2);
          # sanity check
          if($lid ne $seqid){
	    print "WARNING: seqId:[$seqid] and alnSeq:[$alnSeq] don't match!\n";
            next;
	  }
        
         $alnSeq=$lseq; 
         my @alnSeq = split(//, $alnSeq);
         # sanity check
         if(scalar(@alnSeq) != $alnLength){
	     printf "WARNING: the lengths [$alnLength]!=[%d] computed from seqId [$seqid] and alnSeq:\n[$alnSeq]\ndon't match! [CHECK: could be a gap-only column]\n", scalar(@alnSeq);
           }
    
         my ($aCnt,$sCnt)=(0,0); 
         foreach my $as (@alnSeq){
           if(is_nucleotide($as)){
             $seqCoords2alnCoords[$sCnt]=$aCnt;
             $sCnt++;
           }
         $aCnt++;
         }
       } 
       else { 
       printf "WARNING: line number [%d] is meant to correspond to $seqid! Check the formatting.\n", $seqid2positions{$seqid};
       }

       my$mtCnt=0;
       my ($start,$end, $char, $rmfamid) = ($f->{'start'}, $f->{'end'}, $f->{'label'}, $f->{'motif_acc'});

       for (my $gpos=0; $gpos<$fpos; $gpos++){
         my $g = ${ $features->{$seqid} }[$gpos];
         $mtCnt++ if ( overlap($start,$end, $g->{'start'},$g->{'end'}) );
       }

       $motifLines{$seqid}[$mtCnt] = '.' x $alnLength if not defined $motifLines{$seqid}[$mtCnt];
 
       for(my $mpos=$start-1; $mpos<$end; $mpos++){
         my $aCoord = $seqCoords2alnCoords[$mpos];
         substr($motifLines{$seqid}[$mtCnt],$aCoord,1)=$char;

       }
       $fpos++;
      }
      $motiffedSeqLineNumbers{$seqid2positions{$seqid}}=$seqid if defined($motifLines{$seqid}[0]);  
    }
}

#------------------------------------------------------------------------------------
# Print the marked up annotation to file
my $outFile=$fam_dir."tmp.annotated.SEED";
my $fileh = IO::File->new();
$fileh->open("> $outFile");
for(my $ii=0; $ii<scalar(@stk); $ii++){
  if ($ii == $firstSeqLine-1){
    foreach my $l (sort {$motifLabels->{$a} cmp $motifLabels->{$b}} keys %{$motifLabels}){
      if ($l ~~ @allowed_motifs){
        my $label_motif_id = $Motif_hash{$l}->{MOTIF_ID};
        printf $fileh "#=GF MT.%s   %s   %s\n", $motifLabels->{$l}, $l, $label_motif_id; 
      }
    }
  }

  print $fileh $stk[$ii];

  if (defined $motiffedSeqLineNumbers{$ii} && defined $positions2seqid{$ii}){
    my $mCnt=0;
    foreach my $mt (@{$motifLines{$positions2seqid{$ii}}}){
      printf $fileh "#=GR %s MT.$mCnt %s\n", $motiffedSeqLineNumbers{$ii}, $mt if (defined $mt);
      $mCnt++
    }
  }
}

# Run esl-reformat over the annotated file to ensure it is aligned nicely.
my $annotated_SEED=$fam_dir."annotated.SEED";
system "esl-reformat stockholm $outFile > $annotated_SEED";
unlink($outFile);

#-------------------------------------------------------------------------------------
# Subroutine for submitting to cmscan

sub submitToCmscan {
  my ($rfam_acc, $seed, $CMdb, $dir, $tbl) = @_; 

  # Set the search options for cmscan
  my $searchopts     = "--cpu 4 --max --toponly --verbose --cut_ga";
 
  # Set the output files 
  my $jobname = "$$.cmscan.$rfam_acc";
  my $tblO    = $dir.$tbl;
  my $cmsO    = $dir."cmscan";
  my $errO    = $dir."err";

  # Sumbit the job using the cmscan wrapper
  Bio::Rfam::Infernal::cmscan_wrapper($config, $jobname, " --tblout " . $tblO . " " . $searchopts, $CMdb, $seed, $cmsO, $errO, "", "", 0);

  # Change jobname and outname to an array so they can be referenced using wait_for_cluter()
  my (@jobnameAR, @outnameAR);
  push(@jobnameAR, $jobname);
  push(@outnameAR, $tblO);

  # Determine when the job is finished and return the time it took
  my $wait_time = Bio::Rfam::Utils::wait_for_cluster($config->location, $user, \@jobnameAR, \@outnameAR, "# [ok]", "cmscan $rfam_acc", "", "", -1, 1); 
}

#-------------------------------------------------------------------------------------
# Parse the TBL to generate array containing the motif match objects
sub parseTBL2MotifMatchObj{
  my ($TBL, $rfam_acc, @motifs) = @_;

  my @arrayOfMotifMatchObjs;

  open(TBL, "grep -v ^'#' $TBL | sort -nrk 15 | ") || croak "FATAL: could not open pipe for reading $TBL\n[$!]";
  
  while (my $tblline = <TBL>) {
    my ($bits, $evalue, $motif_id, $rfamseq_id, $CMstart, $CMend, $qStart, $qEnd, $strand, $trunc) = processTblOutLineCM($tblline);
  
    # Determine the Motif Accession from the database
    my $rfamlive=$config->rfamlive;
    my $motif_acc_rs = $rfamlive->resultset('Motif')->search({ motif_id => $motif_id });
    my $new_motif_acc = $motif_acc_rs->get_column('motif_acc')->single();  

    # Determine if the motif is in the allowed array. For the first cmscan run this is all of them.
    if ($new_motif_acc ~~ @motifs) {

      # Split rfamseq into acc, start and stop
      my ($rfamseq_acc, $rfamseq_start, $rfamseq_stop);
      if ($rfamseq_id   =~ /(\S+.\d)\/(\d+)-(\d+)/) {
        $rfamseq_acc     = $1;
        $rfamseq_start   = $2;
        $rfamseq_stop    = $3;
      }   
      else { print "Could not understand the sequence identifier $rfamseq_id\n
                    Is it in the correct format?\n";
      }

      # Create the match object
      my $motifMatchObj = Bio::Rfam::MotifMatch->new;
      $motifMatchObj->MOTIF_ACC($new_motif_acc);
      $motifMatchObj->RFAM_ACC($rfam_acc);
      $motifMatchObj->RFAMSEQ_ACC($rfamseq_acc);
      $motifMatchObj->RFAMSEQ_START($rfamseq_start);
      $motifMatchObj->RFAMSEQ_STOP($rfamseq_stop);
      $motifMatchObj->QUERY_START($qStart);
      $motifMatchObj->QUERY_STOP($qEnd);
      $motifMatchObj->MOTIF_START($CMstart);
      $motifMatchObj->MOTIF_STOP($CMend);
      $motifMatchObj->E_VALUE($evalue);
      $motifMatchObj->BIT_SCORE($bits);

      push(@arrayOfMotifMatchObjs, $motifMatchObj)

      }
    }
  return (@arrayOfMotifMatchObjs);
}

#------------------------------------------------------------------------------------------
sub processTblOutLineCM {
  my ($tblline) = @_; 
    my @tblA = split(/\s+/, $tblline);
  my ($motif_name, $qName, $CMstart, $CMend, $qStart, $qEnd, $strand, $trunc, $bits, $evalue) = ($tblA[0], $tblA[2], $tblA[5], $tblA[6], $tblA[7], $tblA[8], $tblA[9], $tblA[10], $tblA[14], $tblA[15]);

  return ($bits, $evalue, $motif_name, $qName, $CMstart, $CMend, $qStart, $qEnd, $strand, $trunc);
}


#-----------------------------------------------------------------------------------------
# Assign labels to each motif to be used in markup of the SEED
sub assign_motif_label {
  my ($motif_id, $taken)=@_;
  my @chars = qw(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z a b c d e f g h i j k l m n o p q r s t u v w x y z 1 2 3 4 5 6 7 8 9 0);
  my @motif_ids = split(//,$motif_id);

  foreach my $c (@motif_ids,@chars) {
    next if ($c !~ /^[a-zA-Z0-9]*$/);
    return $c if (not defined $taken->{$c});
    }
}


#-----------------------------------------------------------------------------------------
sub overlap {
  my($x1, $y1, $x2, $y2) = @_;
  if ( ($x1<=$x2 && $x2<=$y1) || ($x1<=$y2 && $y2<=$y1) || ($x2<=$x1 && $x1<=$y2) || ($x2<=$y1 && $y1<=$y2)  ){
    return 1;
  }
  else {
    return 0;
  }
}

#------------------------------------------------------------------------------------------
sub stockholm2filteredSTK {
    my ($infile,$idf,$fam_dir) = @_;
    system "esl-weight -f --idf $idf $infile > $fam_dir"."filtered.SEED"
	and die "FATAL: failed to run [esl-weight -f --idf $idf $infile > $fam_dir"."filtered.SEED";
    my ($weights,$sum) = stockholm2weights("$fam_dir"."filtered.SEED"); 
    
    return ("$fam_dir"."filtered.SEED",$weights,$sum);
}

#-----------------------------------------------------------------------------------------
sub stockholm2weights {
    my $infile = shift;
    my $sum=0;
    my %weights;
    open(F, "esl-weight -g $infile | ") or die "FATAL: could not open pipe for [esl-weight -g $infile]\n[$!]";
    while(my $w = <F>){
	if($w=~/^#=GS\s+(\S+)\s+WT\s+(\S+)/){
	    $weights{$1}=$2; 
	    $sum+=$2;
	}
    }
return \%weights, $sum;
}

#--------------------------------------------------------------------------------------------
sub stockholmSeqStats{
  my $infile = shift;
  my $seq_num;
  open(F, "esl-seqstat $infile | ") or die "FATAL: could not open pipe [esl-seqstat $infile]\n[$!]";
  while (my $w = <F>) {
    if($w=~/(^Number of sequences:)(\s+)(\d+)/) {
      $seq_num = $3;
     }
  }
  return $seq_num;
}

#---------------------------------------------------------------------------------------------
sub compute_length_of_aligment {    
  my $file = shift;
  my $alnLength;
  open(ALI,"esl-alistat --rna $file |") or die "FATAL: could not open [esl-seqalistat $file] pipe:[$!]";
  while(<ALI>) {
    if (/^Alignment length:\s+(\d+)/){ 
      $alnLength=$1;
    }
  }
  close(ALI);
  print "WARNING: alnLength is undefined for [esl-alistat $file]!" if not defined $alnLength;
  return $alnLength;
}

#-------------------------------------------------------------------------------------------
#returns true if input character is a nucleotide (IUPAC codes):
sub is_nucleotide {
  my $a = shift;
  if (defined($a)){
    $a =~ tr/a-z/A-Z/;
  }  
  if (defined($a) && length($a) && ($a =~ /[ACGUTRYWSMKBDHVN]/) ){
    return 1;}
  else {return 0;}
}

#############################################################################################

sub help {
  print STDERR <<EOF;

motif_scan.pl

RNA Motif Family Scan -  Searches a database of motif CMs (made using cmpress) 
                         against Rfam family SEEDs and populates the Rfam database 
                         with the results linking families to motifs and vice-versa.

Usage:      	motif_scan.pl [options] <RFAM ACC>

Options:	    

	Options
        	-h              : show this help

    	Filter options:
		-fm  <num>      : fraction of sequences a motif must hit before inclusion [DEFAULT: $fractionMotifs]
		-min <num>      : number of sequences a motif must hit before inclusion [DEFAULT: $minNumberHits]
		-idf <num>      : filter out seqs w/ fractional ident > num [DEFAULT: $idf]
	
EOF
}
