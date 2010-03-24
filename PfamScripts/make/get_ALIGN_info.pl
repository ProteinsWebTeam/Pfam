#!/software/bin/perl -w

# This program take the full alignment of a Pfam entry and
# get a shortened view of the information on each sequence
# It writes the information to the file "seq_info" 
#
# Written by rdf, 11/03/03

use strict;

open (ALIGN, "ALIGN") || die "Cannot find the ALIGN file, are in you in the correct directory ?\n";

my %ids;
while (<ALIGN>){
	if ($_ =~ /(\S+)\//){
		$ids{$1}++;
		}
	}

close(ALIGN) || die "Cannot close the ALIGN file\n";

my (@info, $all_ids);
foreach my $id (keys %ids){	
	$all_ids .= " $id";
	}
open(PFETCH, "pfetch -F $all_ids |");
while (<PFETCH>){
	if ($_ =~ /^(DT|OX|RP|RC|RA|SQ|  )\s{3}/){
		next;
		}
	else{
		push(@info, $_);
		}
	}
close (PFETCH);
open(OUTPUT, ">seq_info") || die "could not open seq_info: $!";

foreach (@info){
	chomp ($_);
	print OUTPUT "$_\n";
	}
close(OUTPUT);


