#!/usr/bin/env perl 

# A script to make coverage stats from flatfiles.

# Usage: flatfile_stats.pl <Pfam-A flatfile> <num_seqs> <num_res>

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;


$| = 1;

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );


my $pfama_flat=shift;
#my $pfamb_flat=shift;
my $pfamseq_sequences=shift;
my $pfamseq_residues=shift;

unless($pfama_flat and -s $pfama_flat){ 
  die "Please specify the location of the Pfam-A file.\n";
}

#unless($pfamb_flat and -s $pfamb_flat){ 
#  die "Please specify the location of the Pfam-B file.\n";
#}

unless( $pfamseq_sequences and $pfamseq_sequences =~ /^\d+$/){ 
  die "Please specify the number of sequences in pfamseq.\n";
}

unless( $pfamseq_residues and $pfamseq_residues =~ /^\d+$/){ 
  die "Please specify the number of residues in pfamseq.\n";
}

unless( $pfamseq_residues > $pfamseq_sequences){ 
  die "The number of sequences is greater than the number of residues!!!\n";
}

my ($db,
    $id,
    $nested, 
    $residues, 
    $all_residues, 
    $pfama_sequences,
    $pfama_only_seqs,
    $pfama_residues,
    $pfamAfamilies,
    %all_id,
    %pfama_id,
    %pfamb_id);

open (A, "$pfama_flat") || die"Can't open $pfama_flat\n";
#open (B, "$pfamb_flat") || die"Can't open $pfamb_flat\n";


while (<A>){
    if (/\#=GF\s+ID\s+(\S+)/) {
	$pfamAfamilies++;
#	$A2Blinks = 0;
	next;
    }
    elsif (/\#=GF\s+DR\s+PFAMB/) {
#	$A2Blinks++;
	next;
    }
    elsif (/^\/\//) {
#	$A2Blinknums{ $A2Blinks }++;
	next;
    }
    elsif (/^(\S+)\/(\d+)-(\d+)/){
	#this bit has changed due to nested domains; 
	$id = $1;
	my $start = $2;
	my $end = $3;
	die if (!$start || !$end); 
	if ($pfama_id{$id})
		{
		my $added = 0;
		#go through each start end
		foreach my $se (@{$pfama_id{$id}})
			{
			
			my @coos = split(/-/, $se);
			if (($coos[0] < $start) && ($coos[1] > $end))
				{
				#then previous seq domain was nested
				$nested++;
				my $residues_already_counded = $coos[1] - $coos[0] + 1;
				$residues = $end - $start + 1 - $residues_already_counded;
				$_ = "$start-$end";
				push @{$pfama_id{$id}}, $_;
				$added = 1; 
				last;
				}
			elsif (($coos[0] > $start) && ($coos[1] < $end))
				{
				#then current seq domain is nested
				$nested++;
				#ignore residues as they have already been counted
				$_ = "$start-$end";
				push @{$pfama_id{$id}}, $_;
				$added = 1;
				last;
				}
			}		
		if ($added == 0)
			{
			$residues = $end - $start + 1;
			$_ = "$start-$end";
			push @{$pfama_id{$id}}, $_;
			}
		}	

	else
		{
		$_ =  "$start-$end";
		$residues = $end - $start + 1;
		push @{$pfama_id{$id}}, $_;
		}
	$pfama_residues += $residues;
   	}
	}

close (A);

foreach my $id_a (keys %pfama_id)
	{
	$all_id{$id_a}="A";
	}

$all_residues = $pfama_residues;



#while (<B>){
#    if (/\#=GF\s+ID\s+(\S+)/) {
#	$pfamBfamilies++;
#	$B2Alinks = 0;
#	next;
#    }
#    elsif (/\#=GF\s+DR\s+PFAMA/) {
#	$B2Alinks++;
#	next;
#    }
#    elsif (/^\/\//) {
#	$B2Alinknums{ $B2Alinks }++;
#	next;
#    }
#    elsif (/^(\S+)\/(\d+)-(\d+)\s+(\S+)/){
#	$id = $1;
#	$residues = $3-$2+1;   # Number of residues
#	$pfamb_residues += $residues;
#	if (defined( $all_id{$id})) {
#	    if ($all_id{$id} eq "A") {
#		$all_id{$id} = "Both";
#	    }
#	}
#	else {
#	    $all_id{$id} = "B";
#	}
#
#    }
#}
#close (B);

#$all_residues += $pfamb_residues;


# Now calculate some sequecne coverage figures

my (%count, $nm);
foreach $nm (keys %all_id) {
    $count{ $all_id{$nm} }++;
}

$pfama_only_seqs = $count{"A"};
#$pfamb_only_seqs = $count{"B"};
#$both_a_and_b_seqs = $count{"Both"};
#$all_sequences = $pfama_only_seqs + $pfamb_only_seqs + $both_a_and_b_seqs;
#$all_sequences = $pfama_only_seqs;
#$pfama_sequences = $pfama_only_seqs + $both_a_and_b_seqs;
#$pfamb_sequences = $pfamb_only_seqs + $both_a_and_b_seqs;

my $pfama_seq_cov =  sprintf("%.2f", $pfama_sequences/$pfamseq_sequences*100);
my $pfama_res_cov = sprintf("%.2f", $pfama_residues/$pfamseq_residues*100);

#my $pfamb_seq_cov = sprintf("%.2f", $pfamb_sequences/$pfamseq_sequences*100);
#my $pfamb_res_cov = sprintf("%.2f", $pfamb_residues/$pfamseq_residues*100);

#my $total_seq_cov = sprintf("%.2f", $all_sequences/$pfamseq_sequences*100);
#my $total_res_cov = sprintf("%.2f", $all_residues/$pfamseq_residues*100);

#my $pfamb_add_seq_cov = sprintf("%.2f", $total_seq_cov - $pfama_seq_cov); 
#my $pfamb_add_res_cov = sprintf("%.2f", $total_res_cov - $pfama_res_cov); 


# Now calculate coverages

print "Statistcs for Pfam-A\n";
print "*******************************\n\n\n";

print "\npfamseq:\n\tResidues = $pfamseq_residues\n\tSequences = $pfamseq_sequences\n\n";

print "Pfam-A\n======\n\n";
print "\tNumber of families = $pfamAfamilies\n";
print "\tResidues = $pfama_residues\n";
print "\tSequences = $pfama_sequences (with Pfam-A domains only = $pfama_only_seqs)\n";
print "\tPercent pfamseq residues =",$pfama_res_cov,"\n";
print "\tPercent Pfamseq sequences =",$pfama_seq_cov,"\n\n";

#print "Pfam-B\n======\n\n";
#print "\tNumber of families: $pfamBfamilies\n";
#print "\tResidues: $pfamb_residues\n";
#print "\tSequences: $pfamb_sequences (with Pfam-B domains only = $pfamb_only_seqs)\n";
#print "\tPercent Pfamseq residues =",$pfamb_res_cov,"\n";
#print "\tPercent Pfamseq sequences =",$pfamb_seq_cov,"\n";
#print "\tPercent Pfamseq residues additional =",$pfamb_add_res_cov,"\n";
#print "\tPercent Pfamseq sequences additional =",$pfamb_add_seq_cov,"\n\n";



#print "Combined stats\n===============\n\n";
#print "\tResidues: $all_residues\n";
#print "\tSequences: $all_sequences (with both Pfam-A and Pfam-B domains = $both_a_and_b_seqs)\n";
#print "\tPercent Pfamseq residues =",$total_res_cov,"\n";
#print "\tPercent Pfamseq sequences =",$total_seq_cov,"\n";

#print "\n\nLinks from Pfam-B to Pfam-A\n";
#print "---------------------------\n";

#foreach my $item (sort { $a <=> $b } keys %B2Alinknums) {
#    print "Number of families with $item Pfam-A links: $B2Alinknums{$item}\n";
#}

#print "\nLinks from Pfam-A to Pfam-B\n";
#print "---------------------------\n";

#foreach my $item (sort { $a <=> $b } keys %A2Blinknums) {
#   print "Number of families with $item Pfam-B links: $A2Blinknums{$item}\n";
#}



my $version = $pfamDB->getSchema->resultset('Version')->search()->first;
$version->update({ pfama_coverage => sprintf("%.1f", $pfama_seq_cov),
                   pfama_residue_coverage => sprintf("%.1f", $pfama_res_cov),
                   number_families => $pfamAfamilies });
