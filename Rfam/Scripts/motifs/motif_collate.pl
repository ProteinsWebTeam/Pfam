#! usr/bin/env perl

# motif_collate.pl -    RNA Motif Collate - Takes the matches between the motifs and the sequences in
#                       Family SEED (which are stored in the database from motif_match.pl) and generates
#                       statistics for the mappings.
#

#################################################################################
## Set modules/packages to use

use warnings;
use strict;
use Cwd;
use Getopt::Long;
use IO::File;
use Data::Printer;
use Carp;
use Data::Dumper;
use Text::Table;

use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::Family::MSA;
use Bio::Rfam::Infernal;
use Bio::Rfam::Utils;

use Bio::Easel::MSA;
use Bio::Easel::SqFile;
use Bio::Easel::Random;

#################################################################################
# Set the location of the family seeds
my $motif_STKs = "/nfs/production/xfam/rfam/MOTIFS/results";

# Create a table for the results
my $results_table = Text::Table->new("Rfam Accession",
                                     "Rfam ID",
                                     "Motif Accession",
                                     "Motif ID",
                                     "Matching Sequences",
                                     "Sequences in SEED",
                                     "Fraction",
                                     "Sum of Bit Scores",
                                     "Sum of Weighted Bit Scores"); 

# Set the Rfam config
my $config = Bio::Rfam::Config->new;

# Loop through the results for every family in the DB. TODO: set this as an option.
my $rfamdb = $config->rfamlive;
my @completeFamResultSet = $rfamdb->resultset('Family')->all();

# If -all option is set then:
foreach my $fam (@completeFamResultSet) {
  my $rfam_acc = $fam->rfam_acc;
   
  # Create an array of all the motif_acc that match the family
  my @matching_motifs;

  # Retrieve the matches from the DB for the rfam_acc
  # This code is rubbish but it works.
  my $rs = $rfamdb->resultset('MotifMatch');
  my @rs = $rs -> search( {rfam_acc => $rfam_acc});
  foreach my $match (@rs) {
    my $match_acc =  $match->get_column('motif_acc');
    unless ($match_acc ~~ @matching_motifs) {push(@matching_motifs, $match_acc)}; 
    
  }
  
  # Calculate the GSC tree weights for each sequence in the stockholm alignment
  my $famSTK = "$motif_STKs/$rfam_acc/STK";
  my ($weights, $sum) = stockholm2weights($famSTK);

  # Determine the number of sequences in the SEED of the family
  my $seq_num=stockholmSeqStats($famSTK); 
  
  # For each motif that matches the family, perform the calculations
  foreach my $match_motif (@matching_motifs) {
    my (   $rfam_id,         # Rfam ID eg. 5S_rRNA
           $motif_id,        # Motif ID eg. Terminator1
           $match_num,       # The number of SEED sequences which match the motif
           $match_freq,      # The proportion of SEED seqeunces which match
           $sum_of_bit,      # Sum of the bit scores for the match
           $w_sum_of_bit     # The weighted sum of the bitscores
    ); 

    # Fetch the family and motif ids
    my $motif_id_rs = $rfamdb->resultset('Motif')->single({ motif_acc => "$match_motif"});
    $motif_id=$motif_id_rs->motif_id; 
    $rfam_id=$fam->rfam_id;   
    my $motif_match_rs = $rfamdb->resultset('MotifMatch')->search({ 
                                                   motif_acc => "$match_motif",
                                                   rfam_acc  => "$rfam_acc"}); 
     
    # Calculate the number of sequences from the seed that match the motif
    $match_num=$motif_match_rs->count;

    # Calculate the proportion of sequences from the seed that match motif
    $match_freq=sprintf("%.3f", ($match_num/$seq_num));
    
    # Calculate the Sum of the bitscores and sum of weighted bitscore
    $sum_of_bit = 0.0;
    $w_sum_of_bit = 0.0;
    while (my $match = $motif_match_rs->next) {
      $sum_of_bit+=$match->bit_score;
      my $seq_id = $match->get_column('rfamseq_acc')."/".$match->rfamseq_start."-".$match->rfamseq_stop;
      $w_sum_of_bit+=sprintf("%.1f",($weights->{"$seq_id"})*($match->bit_score));
    } 

    # Add the results to the table
    $results_table->add($rfam_acc,
                        $rfam_id,
                        $match_motif,
                        $motif_id,
                        $match_num,
                        $seq_num,
                        $match_freq,
                        $sum_of_bit,
                        $w_sum_of_bit);
 
  }
}

print $results_table;
#-----------------------------------------------------------
# Specific Modules that utilise easel

# stockholm2weights: Use esl-weights to obtain Gerstein/Sonnhammer/Chothia tree weights for each sequence
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

