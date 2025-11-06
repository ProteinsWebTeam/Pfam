#!/usr/bin/env perl

use strict;
use warnings;

use LWP::UserAgent;
use URI::Escape;

$|=1;

my $dir = shift @ARGV;

my %oc_hash;
my %oc_rank;

my $num=0; # Number of proteins
my $thresh=0.1; # Only print taxa greater than this fraction.

print STDERR "Running $0 on $dir\n";

# Does seq_info file exist?
if (! -e "$dir/seq_info"){
    get_seq_info($dir);
}


my $oc;
open (FH, "$dir/seq_info") or die "Cannot open $dir/seq_info";
while(<FH>){
    if (/^ID/){
	$oc='';
	$num++;
    }
    
    if (/^OC\s+(.*)/){
	$oc.=$1;
    }
    
    if (/^\/\//){
	# Split OC line by ;
	my $rank=0;
	foreach my $element (split (/;/,$oc)){
	    $element =~ s/[\s\.]//;
	    $oc_hash{$element}++;
	    if (! $oc_rank{$element}){
		$oc_rank{$element}=$rank;
	    }
	    $rank++;
	}
    }
    
}
close FH;

if (-e "species"){
    print STDERR "species file exists. SKipping\n";
}

open (OUT, "> $dir/species.tmp") or warn "Cannot write to $dir/species.tmp\n";
open (ANN, "> $dir/speciesann") or warn "Cannot write to $dir/speciesann\n";

my $ann_line='';
foreach my $key (sort keys %oc_hash){
    my $fraction=$oc_hash{$key}/$num;
    if ($fraction>$thresh){
		print OUT "$oc_hash{$key}\t$oc_rank{$key}\t$key\n";


		if ($fraction>0.98 and $oc_rank{$key}<2){
		    $ann_line="CC   Proteins in this family are found in $key.\n";
		} elsif ($fraction>0.8 and $oc_rank{$key}<2){
		    $ann_line="CC   Proteins in this family are primarily found in $key\n";
		} elsif ($fraction>0.1 and $oc_rank{$key}<2) {
		    my $rank=$oc_rank{$key};
		    
		    # Store up proportions and if they are greater than 10% and sum up to over 50% report them in a list.
		    
		}
    }
}

close OUT;

print STDERR $ann_line;
print ANN $ann_line;

system("echo \"Number  Rank    Taxon\" > $dir/species");
system ("sort -nrk2 $dir/species.tmp >> $dir/species");

system("echo \"Total of $num proteins found\" >> $dir/species");

# A subroutine to do equivalent of get_ALIGN_info
sub get_seq_info {
    $dir = shift @_;

    my $n=0;
    open (ALIGN, "$dir/ALIGN") || die "Cannot find the $dir/ALIGN file, are in you in the correct directory ?\n";

    my %ids;
    while (<ALIGN>){
        if ($_ =~ /(\S+)\.\d+\//){
            $ids{$1}++;
        }
    }

    close(ALIGN) || die "Cannot close the $dir/ALIGN file\n";


	# Retrieve Swiss-Prot seq_info file using UniProt REST API
	my @ids = keys %ids;
	my $batch_size = 100;

	open(my $OUTPUT, ">", "$dir/seq_info") or die "Could not open seq_info: $!";

	if (scalar @ids) {
	    print STDERR "Fetching seq_info for ". scalar(@ids) . " sequences, writing to $dir/seq_info file\n";

	    my $ua = LWP::UserAgent->new( timeout => 30 );

		for (my $i = 0; $i < @ids; $i += $batch_size) {
		    my $end = $i + $batch_size - 1;
		    $end = $#ids if $end > $#ids;
		    my @batch = @ids[$i .. $end];

		    my $query = @batch == 1 ? "accession:$batch[0]" : join(" OR ", map { "accession:$_" } @batch);
		    my $url = "https://rest.uniprot.org/uniprotkb/search?format=txt&query=" . uri_escape($query);

		    print STDERR "Fetching batch " . (int($i/$batch_size)+1) . " (" . scalar(@batch) . " entries)\n";

		    my $res = $ua->get($url);
		    if ($res->is_success) {
		        my $content = $res->decoded_content;
		        my @lines = split /\n/, $content;

		        foreach my $line (@lines) {
		            next if $line =~ /^(  )\s{3}/; # Remove sequence lines
		            next if $line =~ /^CC   ---/;
		            next if $line =~ /^CC   Copyrighted/;
		            next if $line =~ /^CC   Distributed/;
		            next if $line =~ /^DT/;
		            print $OUTPUT "$line\n";
		        }
		    }
		    else {
		        warn "Failed to fetch batch starting at $batch[0]: " . $res->status_line . "\n";
		    }

		    select(undef, undef, undef, 0.25); # pause between batches
		}
	} else {
	    print STDERR "No sequences to fetch — created empty $dir/seq_info file\n";
	}

	close $OUTPUT;


}


