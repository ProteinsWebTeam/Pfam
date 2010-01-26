#! /software/bin/perl -w

use strict;
use Getopt::Long;

#Script for calculating amino acid coverage for a sequence database
#Before running this script you need to create two files:
#length_file.  This file contains the lengths of all the sequences in the database. 
#The format of the length file is auto, length (e.g. select auto_metaseq, length from metaseq)
#regions_file.  This file contains all of the pfamA regions found in on sequences in the database.  
#The format of the regions file is auto_pfamA, start, end (e.g. select auto_pfamA, seq_start, seq_end from meta_pfamA_reg)
#The regions file MUST be sorted by auto_pfamA (e.g. sort -n <regions_file>)


my ($regions_file, $length_file);
GetOptions( 'regions_file=s' => \$regions_file,
	    'length_file=s'  => \$length_file);

unless($regions_file and -s $regions_file) {
    die "Need to specify a regions file on the command line\n";
}

unless($length_file and -s $length_file) {
    die "Need to specify a length file on the command line\n";
}

print STDERR "Parsing $length_file...";

my $length=0;
open(LEN, $length_file) or die "Couldn't open $length_file $!";
while(<LEN>) {
    if(/^\S+\s+(\S+)/) { 
	$length+=$1;
    }
}
close LEN;

print STDERR "done\n";


print STDERR "Parsing $regions_file...";

my %reg;
my $aa=0;
open(REG, $regions_file) or die "Couldn't open $regions_file $!";

my $a;
while(<REG>) {
    if(/^(\S+)\s+(\S+)\s+(\S+)/) {
        my ($auto, $start, $end) = ($1, $2, $3);

	if($a) {
	    if($a ne $auto) {
		$aa+=calculate(\%reg, $a);
		$a=$auto;
	    }
	}
	else {
	    $a=$auto;
	}

        for(my $i=$start; $i <= $end; $i++) {
	    $reg{$auto}[$i-1]=1;
          
	}
    }
}
close REG;
$aa+=calculate(\%reg, $a);

close REG;

print STDERR "done\n";


print "Number of residues covered by Pfam-A:$aa\nTotal number of residues:$length\n";
my $coverage = ($aa/$length)*100;
print "Amino acid coverage is $coverage\%\n";

sub calculate {
    my ($reg, $auto)= @_;

    my $aa;

    foreach my $element (@{$reg{$auto}}) {
	if($element) {
	    $aa++;
	}
    }

    delete $$reg{$auto};

    return($aa);   
}
