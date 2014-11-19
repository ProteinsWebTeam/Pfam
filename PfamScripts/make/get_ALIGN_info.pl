#!/usr/bin/env perl

# This program take the scores file of a Pfam entry and
# get a shortened view of the information on each sequence above a specified PE to a specified max
# It writes the information to the file "seq_info" 
#
# Written by rdf, 11/03/03
#updted by ruthe, 19/11/14

use strict;
use warnings;
use DDP;
use Getopt::Long;

my ($total, $number, $pe, $help);
&GetOptions('n=i' => \$number,
	    'p=i'=> \$pe,
	    'h' => \$help
    );

if($help){
    &help;
    exit;
}

#default of 100 entries. need to add control that number is an integer from 0 - 10,000(?)
if ($number && $number < 1){
    print "Number (n) must be between 1 and 10,000, so the default value of 1,000 is being used\n";
    $number = 1000;
} elsif ($number && $number > 10000){
    print "Number (n) must be between 1 and 10,000, so the default value of 1,000 is being used\n";
    $number = 1000;
} elsif (!$number){
    print "Number (n) has not been specified, so 1,000 entries will be retrieved\n";
    $number = 1000;
}

if ($pe && $pe < 1){
    print "PE (p) must be an integer between 1 and 5, so the default value of 3 is being used\n";
    $pe = 3;
} elsif ($pe && $pe > 5){
    print "PE (p) must be an integer between 1 and 5, so the default value of 3 is being used\n";
    $pe = 3;
} elsif (!$pe){
    print "PE (p) has not been specified, so the default value of 3 is being used\n";
    $pe = 3;
}

open (SCORES, "scores") or die "Cannot open the scores file\n";
open (OUTFILE, ">seq_info") or die "Cannot open seq_info file to write\n";

my %ids;
while (<SCORES>){
    if ($_ =~ /\d+\.\d+\s+(\w+\.\d+)\//){
	$ids{$1}++;
    }
}

close(SCORES) || die "Cannot close the scores file\n";

#split ids into batches of 500
#need them in an array
my @accns;
foreach my $acc (keys %ids){
    push (@accns, $acc);
}

#testing with 20 - will set at 500
my $chunksize = 20;
my $numremain = $number;

while (@accns){
    if ($numremain > 0){
    my @subaccns = splice @accns, 0, $chunksize;

    my ($data, $count)= fetch(\@subaccns,$numremain,$pe);

    print OUTFILE $data;
    $numremain = $numremain - $count;
    } else {
	exit;
    }

}

close (OUTFILE) || die "Can't close seq_info file\n";

####SUBROUTINES#####

sub fetch {
    my ($accn_ref, $num, $p) = @_;
    my $data = "";
    my $count = 0;
    my $all_accns;
    my $entries;

    foreach my $acc (@{$accn_ref}){
	$all_accns .= " $acc";
    }
    open(PFETCH, "pfetch -F $all_accns |");
    while (<PFETCH>){
	    if ($_ =~ /^(DT|OX|RP|RC|RA|SQ|  )\s{3}/){
		next;
	    } elsif ($_ =~ /^CC\s{3}-|^CC\s{3}Copy|^CC\s{3}Dist/){
		next;
	    }else {
		$entries .= $_;
	    }
    }
    close (PFETCH);

#split entries into individual uniprot entries
    my @entry_split = split (/\/\/\n/, $entries);
    foreach my $uniprot (@entry_split){
	my $ev;
	my $entry;
	my @lines = split (/\n/, $uniprot);
	foreach my $line (@lines){
	    if ($line =~ /^PE\s{3}(\d)/){
		$ev = $1;
		$entry .= $line . "\n";
	    } else {
		$entry .= $line . "\n";
	    }
	}

	if ($ev && $ev <= $p && $count < $num){
	    $data .= $entry . "\\\\\n";
	    $count ++;
	}

    } #end of loop through each uniprot
 
    return $data, $count;
}

sub help{
    print "****** get_ALIGN_info..pl - Create a seq_info file from a Pfam scores file ******\n";
    print "Usage:\n\tget_ALIGN_info.pl -n <> -p <>\n";
    print "Options:\n";
    print "\t-n\t\tNumber of entries to retrieve (default is 1000)\n";
    print "\t-p\t\tMaximum PE value of retrieved entries (default is 3)\n";
    print "To be run in the family directory. Requires scores file to exist\n";
}

