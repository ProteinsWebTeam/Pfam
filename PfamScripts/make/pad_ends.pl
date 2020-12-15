#!/usr/bin/env perl

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;

my ($aln, $max_pad, $help);


&GetOptions("align=s" => \$aln,
			"max_pad=i" => \$max_pad,
            "help" => \$help);

unless($aln and -s $aln) {
    help();
}

$max_pad = 10 unless($max_pad);

my $config = Bio::Pfam::Config->new;
my $db_dir = $config->{pfamseq}->{location};
#my $pfamseq = "$db_dir/pfamseq";
my $uniprot = "$db_dir/uniprot";

#Find max length of accession/start-end in the alignment
my $max_length = max_acc_length($aln);
$max_length += 4; #Need to leave a gap between acc/st-en and aligned seq, and allow for st/en increasing in length by one digit

my $count =0;
my %done;
open(ALN, $aln) or die "Couldn't open fh to $aln, $!";
while(<ALN>) {
    if(/^((\S+)\/(\d+)-(\d+))\s+(.+)$/) {
        my ($acc_se, $seq_acc, $st, $en, $aligned_sequence) = ($1, $2, $3, $4, $5); 

        #Removed annoying whitespace that is sometimes at the end of the alignment
        if($aligned_sequence =~ /(\S+)\s+$/) {
            $aligned_sequence = $1;
        }
        
        #Get full length sequence
		my $sequence = get_sequence($uniprot, $seq_acc);
		
        #Print sequence as is if not in current database
		unless($sequence) {
            if(exists($done{$acc_se})) {
                print STDERR "Removing duplicate $acc_se\n";
            }
            else {
                printf("%-${max_length}s"."$aligned_sequence\n", $acc_se);
            }
            $done{$acc_se}=1;
			next;
		}

        #Pad the sequence at N and C terminal if required
        my ($new_start, $new_end);
        my $full_length = length($sequence);

        unless($st == 1) {
            ($new_start, $aligned_sequence) = n_terminal_pad($aligned_sequence, $sequence, $st, $max_pad);
        }
        unless($en == $full_length) {
            ($new_end, $aligned_sequence) = c_terminal_pad($aligned_sequence, $sequence, $en, $max_pad);
        }

        if($new_start or $new_end) { #If the sequence was padded
            $st = $new_start if($new_start);
            $en = $new_end if($new_end);
            
            print STDERR "Changing $acc_se to ";
            $acc_se = "$seq_acc/$st-$en";
            print STDERR "$acc_se\n";

            #Lets double check the sequence segment we have created is correct
            #It would be very bad if it was incorrect...
            my $seq1 = $aligned_sequence;

            $seq1 =~ s/\-//g;
            $seq1 =~ s/\.//g;
            my $l = $en - $st + 1;
            my $seq2 = substr($sequence, $st-1, $l);
            unless($seq1 eq $seq2) {
                die "Couldn't correctly get the sequence ($acc_se)\nGot:      [$seq1]\nExpected: [$seq2]\n";
            }
            $count++;
        }
        
        #Print out acc/st-en and aligned sequence
        if(exists($done{$acc_se})) {
            print STDERR "Removing duplicate $acc_se\n";
        }
        else {
            printf("%-${max_length}s"."$aligned_sequence\n", $acc_se);
        }
        $done{$acc_se}=1;
    }
    else {
        unless(/^\/\/$/) {
            print STDERR "Not adding this line to alignment: $_";
        }
    }
}
close ALN;
print STDERR "$count sequences were padded\n";

sub max_acc_length {
    #Get maximum length of acc/st-en
    my ($aln) = @_;
    
    open(A, $aln) or die "Couldn't open fh to $aln, $!";
    my $max_length = 0;
    while(<A>) {
        if(/(\S+\/\d+-\d+)/) {
            my $length = length($1);

            if($length > $max_length) {
                $max_length = $length;
            }
        }
    }
    close A;

    if($max_length > 0) {
        return($max_length);
    }
    else {
        die "Unable to get the maximum length of accession/start-end from $aln";
    }
}

sub get_sequence {
    #Get full length sequence from fasta file
    my ($db, $seq_acc) = @_;
    open(SEQ, "esl-sfetch $db $seq_acc |") or die "Coudn't open fh to 'esl-sfetch $db $seq_acc', $!";
    my $sequence;
    while(<SEQ>) {
        unless(/^>/) {
            chomp $_;
            $sequence .= $_
        }
    }
    close SEQ;
	
	if($sequence) {
		return($sequence);
	}		
	else {
		print STDERR "warn - $seq_acc is not in the current sequence database ($db)\n";
	}
}


sub n_terminal_pad {
    my ($aligned_sequence, $sequence, $start, $max_pad) = @_;

    my $new_start;
    if($aligned_sequence =~ /^([.-]+)[A-Za-z]/) {
        my $n_pad = length($1);
        if($n_pad <= $max_pad) {
            if($n_pad >= $start) {
                $new_start = 1;
                $n_pad = $start - 1;
            }
            else {
                $new_start = $start - $n_pad;
            }

            #Get the residues to add to N terminal
            my $n_res = substr($sequence, $new_start-1, $n_pad);

            #Remove the '-' at the N terminal
            substr($aligned_sequence, 0, $n_pad, "");

            #Add the missing residues to the N terminal
            $aligned_sequence = $n_res . $aligned_sequence; 

        }
    }
    return($new_start, $aligned_sequence);
}


sub c_terminal_pad {

    my ($aligned_sequence, $sequence, $end, $max_pad) = @_;
    my $new_end;
    if($aligned_sequence =~ /[A-Za-z]([.-]+)$/) {
        my $c_pad = length($1);
        if($c_pad <=  $max_pad) {
            if( ($c_pad + $end) > length($sequence)) {
                $c_pad = length($sequence) - $end;
                $new_end = length($sequence);
            }
            else {
                $new_end = $end + $c_pad;
            }

            #Get the residues to add to C terminal
            my $c_res = substr($sequence, $end, $c_pad);

            #Remove '-' at the C terminal
            my $i = length($aligned_sequence) - $c_pad;
            substr($aligned_sequence, $i, $c_pad, ""); 
 
            #Add the missing residues to the C terminal
            $aligned_sequence .= $c_res;  
            
        }
    }
    return($new_end, $aligned_sequence);
}


sub help {
print<<EOF;

This script can be used to 'pad' missing residues at the N and C termini 
of alignments. The sequences must be in the current pfamseq or uniprot 
used for Pfam.

Usage:
	$0 -align <alignment>

Options:
    -max_pad <integer>   maximum number of residues to pad at N/C ends (default 10)
    -help                shows this help message
EOF
    exit(1);
}
