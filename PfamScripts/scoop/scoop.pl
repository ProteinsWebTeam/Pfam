#! /usr/local/bin/perl -w

# Software to find matches between Pfam families.
#
# Basically the method compares two HMM output files and finds the
# number of common regions between the two. Two regions are counted as
# common if one of their midpoints lies within the other.  
#
# To use the software an input file must be provided. An example is
# shown below:
#
# O00050 PF06465  448     500     2.5e-31
# O00050 PF07453  19      52      1.8e+03
# O00050 PF07572  89      159     9.8e+02
# O00053 PF00012  37      642     0.0
# O00053 PF00416  507     576     3.9e+02
# O00053 PF00630  167     264     2.6e+02
# O00053 PF00929  37      180     8.8e+02
#
#
# The columns are as follows:
#  column 1 protein identifier
#  column 2 pfam family accession
#  column 3 start of match
#  column 4 end of match
#  cloumn 5 E-value of match
#
# NB The file must be sorted by the first column!
#
# An example of the output is shown here:
#
# 15.1651888357382 PF00001 23432 PF00002 8904 Both 3639
# 5.48080433630521 PF00001 23432 PF00003 23851 Both 3508
# 0.00261500319008754 PF00001 23432 PF00004 20335 Both 1
# 0.00396570101830259 PF00001 23432 PF00005 26835 Both 2
# 2.00941226589356 PF00001 23432 PF00007 1008 Both 40
# 0.00354743435012688 PF00001 23432 PF00009 14976 Both 1
# 0.0153235629090441 PF00001 23432 PF00010 3426 Both 1
# 0.127958318941515 PF00001 23432 PF00014 2030 Both 5
#
#
# The columns of the output are as follows:
#
# Column 1: SCOOP normalised score. Scores greater than 50 are very likely to be true matches.
# Column 2: Family 1 identifier (Pfam accession in this case).
# Column 3: Number of matches in total in family 1.
# Column 4: Family 2 identifier (Pfam accession in this case).
# Column 5: Number of matches in total in family 2.
# Column 6: Number of common matches between family 1 and family 2.

use strict;
use warnings;
use Bio::SCOOP::Region;
use Bio::SCOOP::RegionSet;
use Getopt::Long;

my $non_redundant=1; # Set to true to filter matches for redundancy using score

my $file = shift @ARGV;
if (! $file){
    die "Usage $0: <data file>";
}

print STDERR "Loading up regions\n";
my $total_region=0;
my %family_total; # Counts matches per family
my %uber_matrix;


open (FH, $file) or die "Cannot open $file";
my $old_id;
my ($set,$new_set);
foreach my $line (<FH>){
    chomp $line;
    my @line=split(/\s+/,$line);
    my $id=$line[0];

    # Make new region object
    my $region = new Bio::SCOOP::Region('id'=>$id,
			    'start'=>$line[2],
			    'end'=>$line[3],
			    'family'=>$line[1],
			    'evalue'=>$line[4],
			    'significant'=>$line[5]);

    if ($id ne $old_id and $old_id){
	# Make new set
	$new_set=new Bio::SCOOP::RegionSet($id);
	$new_set->add($region);
	$total_region++;
	
	# Now add old set to uber_matrix
	add_set_to_matrix($set,\%family_total,\%uber_matrix);
	# delete set
	undef $set;

	$set=$new_set;
    } elsif ($id eq $old_id) {
	$set->add($region);
	$total_region++;
    } elsif ($id ne $old_id and ! $old_id){
	$new_set=new Bio::SCOOP::RegionSet($id);
	$new_set->add($region);
	$total_region++;

	# No old set as this is the first protein
	# No old set to delete

	$set=$new_set;
    }
    $old_id=$id;
}
print STDERR "Finished processing regions\n";


# OK now have all regions loaded up
# Loop over each protein and populate uber matrix.


# This routine takes a RegionSet and does comparison and adds data to uber_matrix
sub add_set_to_matrix {
    my ($set,$family_total,$uber_matrix)=@_;

    my @regions=$set->each();
    my $n=@regions;

    #print STDERR "Looking at protein $id with $n regions\n";
    # Now loop over diagonal half matrix
    if ($n == 1){
	$family_total->{$regions[0]->family()}++; # Still add to family counts
	return();
    }
    
    for (my $i=0;$i<$n;$i++){
	# For every region add to family count
	if (! $regions[$i]){
	    warn "No region for $i\n"
	}

	$family_total->{$regions[$i]->family()}++;

	for (my $j=$i+1;$j<$n;$j++){
	    if (! $regions[$j]){
		warn "No region for $j\n";
	    }
	    # Are regions the same?
	    if ($regions[$i]->overlap($regions[$j])){
		# Test whether families are the same - should never happen!
		# Now happens with HMMER3!
		my $die_with_internal_overlap=0;
		if ($die_with_internal_overlap){
		    if ($regions[$i]->family() eq $regions[$j]->family()){
			print STDERR "REGIONS OVERLAP\n";
			$regions[$i]->write();
			$regions[$j]->write();
			
			die "Two regions in the same protein overlap in the same family. This should never happen!";
		    }
		}

		# Also test whether either region has counted towards a match between these families already.
		if (! $regions[$i]->match($regions[$j]->family()) and ! $regions[$j]->match($regions[$i]->family())){

		    # Finally test whether score has been seen before
		    if ($non_redundant){
			if (! $regions[$i]->score($regions[$j]->evalue()) and ! $regions[$j]->score($regions[$i]->evalue())){
			    $uber_matrix->{$regions[$i]->family()}{$regions[$j]->family()}++;

			    $regions[$i]->add_score($regions[$j]->evalue());
			    $regions[$j]->add_score($regions[$i]->evalue());
			}
		    } else {
			$uber_matrix->{$regions[$i]->family()}{$regions[$j]->family()}++;

		    }

		    # Add to list of outputs matched in region object
		    $regions[$i]->add_match($regions[$j]->family());
		    $regions[$j]->add_match($regions[$i]->family());

		}
	    }
	}
    }
    return();
}

# Now do final processing
print STDERR "Total regions:$total_region\n";

# Variables used to calculate the average score per family
my %family_score_total;
my %family_score_denominator;
# Loop over every pair of families in uber matrix and calculate raw score
open (FH, "> old_scores")or die "cannot write old_scores";
foreach my $x (sort keys %uber_matrix){
    foreach my $y (sort keys %{$uber_matrix{$x}}){
	my $expected=1+$family_total{$x}*$family_total{$y}/$total_region;
	my $observed=$uber_matrix{$x}{$y};
	my $test=0;
	my $score;
	if (! $test){
	    $score=$observed/$expected;
	} else {
	    $score=$uber_matrix{$x}{$y};
	}
	# print out raw scores
	print FH "$score $x $family_total{$x} $y $family_total{$y} Both $observed\n";

	# Store for each family the total score and the number of entries.
	$family_score_total{$x}+=$score;
	$family_score_total{$y}+=$score;
	$family_score_denominator{$x}++;
	$family_score_denominator{$y}++;
    } 
}

# Print out average score data - used to calculate normalised scores
foreach my $x (keys %family_score_total){
# Method d
    my $average_score=$family_score_total{$x}/(100+$family_score_denominator{$x});
    print FH "# Average score for $x is $average_score denominator is $family_score_denominator{$x}\n";
}
print FH "# TOTAL REGIONS $total_region\n";
close FH;

# Loop over every pair of families in uber matrix and calculate normalised score
foreach my $x (sort keys %uber_matrix){
    foreach my $y (sort keys %{$uber_matrix{$x}}){
	my $average_score_x=$family_score_total{$x}/(100+$family_score_denominator{$x});
	my $average_score_y=$family_score_total{$y}/(100+$family_score_denominator{$y});
	my $max_score;
	if ($average_score_x>$average_score_y){
	    $max_score=$average_score_x;
	} else {
	    $max_score=$average_score_y;
	}

	# The below line is current best method
	my $expected=1+$max_score*$family_total{$x}*$family_total{$y}/$total_region;
	my $observed=$uber_matrix{$x}{$y};
	my $test=0;
	my $score;

	$score=$observed/($expected);

	print "$score $x $family_total{$x} $y $family_total{$y} Both $observed\n";
    } 
}
