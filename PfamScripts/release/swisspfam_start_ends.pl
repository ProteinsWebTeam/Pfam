#!/usr/bin/env perl

use strict;
#print STDERR "This script\n";
my ($name, $desc, $acc, $seqs, %seqaccs, %gianthash, %seqAccDotVersion);

my ($pfamseqlen, @megalist);

$pfamseqlen = shift;
open PFAMSEQ, "$pfamseqlen" or die "Could not open $pfamseqlen";

# First, pre-process the flat-files(s)

while (<>) {
    /^\#=GF\s+ID\s+(.*)/ && do {
	$name = $1;
	$desc = "";
	%seqaccs = ();
	next;
    };
    /^\#=GF\s+DE\s+(.*)/ && do {
	$desc = $1;
	next;
    };
    /^\#=GF\s+SQ\s+(\d+)/ && do {
	$seqs = $1;
	next;
    };
    /^\#=GF\s+AC\s+(.*)/ && do {
	$acc = $1;       # this is Pfam acc not sequence acc!
	next;
    };
    /^\#=GS\s+(\S+)\/\d+\-\d+\s+AC\s+(\S+)/ && do {
	my $id = $1;
	my $accDotVersion = $2;
	my ($accNoVersion, $version) = split(/\./, $accDotVersion);
	#print STDERR "*** $accNoVersion, $accDotVersion ***\n";
	$seqAccDotVersion{$accNoVersion} = $accDotVersion;
	$seqaccs{$id} = $accDotVersion;
	next;
    };
    /^(\S+)\/(\d+)\-(\d+)/ && do {
	my ($nm, $st, $en)  = ($1, $2, $3);
	my $seq_acc = $seqaccs{ $nm };
	push @megalist, "$nm $seq_acc $st $en $name $seqs $acc $desc"; 
	next;
    };
	
}
@megalist = sort by_name @megalist;


while (<PFAMSEQ>) {
    /^(\S+)\s+(\d+)/ && do {
	my $accl = $1;  # this is now a sequence accession
	my $len  = $2;
	
	$gianthash{$seqAccDotVersion{$accl}} = $len;
    };
}

foreach my $el (@megalist) {
    my ($seqname, $seqacc, $end) = ($el =~ /^(\S+)\s+(\S+)\s+\d+\s+(\d+)/);
    if( not exists $gianthash{$seqacc} ) {
	print STDERR "Error: can't get a length for [$seqacc] [$el]\n";
    }
    elsif ($gianthash{$seqacc} < $end) {
	print STDERR "Error: length less than end for [$seqacc] [$el]\n";
    } else {
        print "$gianthash{ $seqacc } $el\n";
    }
} 



############## sub-routines #############################

sub by_name {
    my ($seqname1, $domname1) = $a =~ /^(\S+)\s+\S+\s+\d+\s+\d+\s+(\S+)/;
    my ($seqname2, $domname2) = $b =~ /^(\S+)\s+\S+\s+\d+\s+\d+\s+(\S+)/;

    my $retval =  $seqname1 cmp $seqname2 || $domname1 cmp $domname2;

    return $retval;
}
