#! /software/bin/perl -w

#use this script to generate start ends file for mkswissPfam program
#for metagenomics and genpept data 
#need to pass lengths file and Pfam-A.full on command line 
#e.g.  $0 genpept_len Pfam-A.full.ncbi


use strict;

my ($name, $desc, $acc, $seqs, %seqaccs, %gianthash);

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
	$seqaccs{$1} = $2;
	next;
    };
    /^(\S+)\/(\d+)\-(\d+)/ && do {
	my ($nm, $st, $en)  = ($1, $2, $3);
	push @megalist, "$nm $nm $st $en $name $seqs $acc $desc"; 
	next;
    };
	
}
@megalist = sort by_name @megalist;


while (<PFAMSEQ>) {
    /^(\S+)\s+(\d+)/ && do {
	my $accl = $1;  # this is now a sequence accession
	my $len  = $2;
	$gianthash{$accl} = $len;
    };
}

foreach my $el (@megalist) {
    my ($seqname, $end) = ($el =~ /^(\S+)\s+\S+\s+\d+\s+(\d+)/);
    if( not exists $gianthash{$seqname} ) {
	print STDERR "Error: can't get a length for [$seqname] [$el]\n";
    }
    elsif ($gianthash{$seqname} < $end) {
        print STDERR "Error: length less than end for [$seqname] [$el]\n";
    } else {
        print "$gianthash{ $seqname } $el\n";
    }
} 



############## sub-routines #############################

sub by_name {
    my ($seqname1, $domname1) = $a =~ /^(\S+)\s+\S+\s+\d+\s+\d+\s+(\S+)/;
    my ($seqname2, $domname2) = $b =~ /^(\S+)\s+\S+\s+\d+\s+\d+\s+(\S+)/;

    my $retval =  $seqname1 cmp $seqname2 || $domname1 cmp $domname2;

    return $retval;
}
