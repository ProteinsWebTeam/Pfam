#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;

#This script should be run after pud-findOverlapsAndMissing.pl
# 1. This script allocates each overlap in the <date>overlaps.overlaps.filtered to the overlapping family with the least number of 
#    overlaps. The number of overlaps per family is taken from <date>.overlaps.familyOverlaps.filtered file
# 2. The ignore_threshold sets the maximum number of overlaps per family that will be filtered out
#    of the overlap file
# 3. If an overlap is allocated to a family which has > ignore_threshold, it will be printed out, else the overlap will
#    be filtered out
# 4. The script will also look for an 'ignore_list' file in the overlaps dir and filter out any overlaps between the pairs
#    of families in this file (it looks for pairs of PFXXXXX numbers on each line in this file)
# 5. This script produces two files in the overlaps directory: <date>overlaps.allocated_ignore and <date>overlaps.familyOverlaps.allocated_ignore

my ($date, $overlaps_dir);

GetOptions('date=s'  => \$date,
            'dir=s'  => \$overlaps_dir);

unless($date and $overlaps_dir) {
    die "Need to specify date of overlap file on command line\nEg $0 -date 20210107 -dir /nfs/production/xfam/pfam/sequpd/34.0/overlapLogs\n";
}

my $overlaps_file = $overlaps_dir . "/" . $date . "overlaps.overlaps.filtered";
my $family_overlaps_file = $overlaps_dir . "/" . $date . "overlaps.familyOverlaps.filtered";

unless(-s $family_overlaps_file and -s $family_overlaps_file) {
    die "Check overlaps file [$family_overlaps_file] and family overlaps file [$family_overlaps_file] exist";
}

my $ignore_threshold = 3; #Maximum number of overlaps per family that will be filtered out of the overlaps file


#Read family overlaps file and store how many overlaps for each family
#Also store pfamA_id, num_seed, and num_full for each family
my ($total_overlaps, $pfamA_id, $pfamA_id_seed_full) = read_family_overlaps($family_overlaps_file);


#Read ignore list file
my $ignore_list = "$overlaps_dir/ignore_list";
my $ignore_fams;
if(-s $ignore_list) {
    $ignore_fams =  read_ignore_list($ignore_list);
}


#Read overlaps file and assign each overlap to family with least overlaps
#$count hash contains number of overlaps per family
my ($count) = assign_overlaps($overlaps_file, $total_overlaps);


#Read in overlaps file again, and only print those that have more than ignore_threshold overlaps
my $overlaps_outfile = "$overlaps_dir/$date" . "overlaps.allocated_ignore";
#$new_count hash contains new number of overlaps per family after filtering overlaps which are allocated to a family wih <= ignore_threshold overlaps
#overlapping_fams hash contains a list of overlapping families for each family with overlaps
my ($new_count, $overlapping_fams) = print_overlaps($overlaps_file, $overlaps_outfile, $count, $total_overlaps, $ignore_threshold, $ignore_fams, $pfamA_id);


#Make a new family overlaps file
my $family_overlaps_outfile = "$overlaps_dir/$date" . "overlaps.familyOverlaps.allocated_ignore";
open(NEWFO, ">$family_overlaps_outfile") or die "Couldn't open fh to $family_overlaps_outfile, $!";
print NEWFO "acc     id                      NoSeed          NoFull          NoOverlaps      OverlapFams\n";
foreach my $pfamA_acc (sort { $new_count->{$b} <=> $new_count->{$a} } keys %$new_count) {
    print NEWFO "$pfamA_acc $pfamA_id_seed_full->{$pfamA_acc}\t\t$new_count->{$pfamA_acc}\t\t$overlapping_fams->{$pfamA_acc}\n";
}


sub read_family_overlaps {
    my ($fam_overlaps_file) = @_;

    my (%pfamA_id_seed_full, %pfamA_id, %total_overlaps);
    open(F, $fam_overlaps_file) or die "Couldn't open fh to $fam_overlaps_file, $!";
    while(<F>) {
        if(/^acc/) {
            next;
        }
        elsif(/^(\S+)\s+((\S+)\s+\d+\s+\d+)\s+(\d+)/) {  #PF00098 zf-CCHC     79      57349   249    PF19088:TUTase,PF10159:MMtag,PF00076:RRM_1,
            my $pfamA_acc = $1;
            $pfamA_id_seed_full{$pfamA_acc}=$2;
            $pfamA_id{$pfamA_acc}=$3;
            $total_overlaps{$pfamA_acc}=$4;
        }
        else {
            print STDERR "Couldn't parse line: $_";
        }
    }
    close F;

    return(\%total_overlaps, \%pfamA_id, \%pfamA_id_seed_full);
}

sub read_ignore_list {

    my ($ignore_file) = @_;
    my %ignore;
    open(IGNORE, $ignore_file) or die "Couldn't open fh to $ignore_file, $!";
    while(<IGNORE>) {
        if(/(PF\d{5}).+(PF\d{5})/) {
            my ($fam1, $fam2) = ($1, $2);
            $ignore{"$fam1:$fam2"}=1;
            $ignore{"$fam2:$fam1"}=1;
            print STDERR "Ignoring overlaps between $fam1 and $fam2\n";
        }
    }
    close IGNORE;

    return(\%ignore);
}

sub assign_overlaps {
    my ($overlaps_file, $total_overlaps, $ignore_fams) = @_;
    my %count;
    open(O, $overlaps_file) or die "Couldn't open fh to $overlaps_file, $!";
    while(<O>) {
        if(/(PF\d{5}).+SEED.+(PF\d{5}).+SEED/) {  #Seed-seed overlap will be assigned to first family
            my ($fam1, $fam2) = ($1, $2);
            $count{$fam1}++;
        }
        elsif(/(PF\d{5}).+(PF\d{5})/) {
            my ($fam1, $fam2) = ($1, $2);

            next if(exists($ignore_fams->{"$fam1:$fam2"}));

            if($total_overlaps->{$fam1} < $total_overlaps->{$fam2}) {
                $count{$fam1}++;
            }
            else {
                $count{$fam2}++;
            }
        }
        else {
            die "Couldn't parse line in $overlaps_file:\n$_";
        }
    }
    close O;
    return(\%count);
}

sub print_overlaps {
    my ($overlaps_file, $overlaps_outfile, $count, $total_overlaps, $ignore_threshold, $ignore_fams, $pfamA_id) = @_;
    my (%new_count, %overlapping_fams, %added);
    open(O, $overlaps_file) or die "Couldn't open fh to $overlaps_file, $!";
    open(NEWO, ">$overlaps_outfile") or die "Couldn't open fh to $overlaps_outfile, $!";
    while(<O>) {
        if(/(PF\d{5}).+SEED.+(PF\d{5}).+SEED/) { #Seed-seed overlap will be assigned to first family
            my ($fam1, $fam2) = ($1, $2);
            $new_count{$fam1}++;
            unless(exists($added{"$fam1:$fam2"})) {  #Store all the families it overlaps with, but don't store a family more than once
                $overlapping_fams{$fam1}.="$fam2:$pfamA_id->{$fam2}".",";
                $added{"$fam1:$fam2"}=1;
            }
            print NEWO $_;
        }
        elsif(/(PF\d{5}).+(PF\d{5})/) {
            my ($fam1, $fam2) = ($1, $2);

            next if(exists($ignore_fams->{"$fam1:$fam2"}));
            
            if($total_overlaps->{$fam1} < $total_overlaps->{$fam2}) { #fam1 has less overlaps, so gets allocated this overlap
                if($count->{$fam1} > $ignore_threshold) {
                    $new_count{$fam1}++;
                    print NEWO $_;
                    unless(exists($added{"$fam1:$fam2"})) { #Store all the families it overlaps with, but don't store a family more than once
                        $overlapping_fams{$fam1}.="$fam2:$pfamA_id->{$fam2}".",";
                        $added{"$fam1:$fam2"}=1;
                    }
                }
            }
            else { #fam2 is allocated the overlap
                if($count->{$fam2} > $ignore_threshold) {
                    $new_count{$fam2}++;
                    print NEWO $_;
                    unless(exists($added{"$fam2:$fam1"})) {  #Store all the families it overlaps with, but don't store a family more than once
                        $overlapping_fams{$fam2}.="$fam1:$pfamA_id->{$fam1}".",";
                        $added{"$fam2:$fam1"}=1;
                    }
                }
            }
        }
        else {
            die "Couldn't parse families from this line in $overlaps_file:\n$_";
        }
    }
    close O;
    close NEWO;

    return(\%new_count, \%overlapping_fams);
}
