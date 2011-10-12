# calculate sensitivity based on the
# output of OutputStatisticsClustering

use strict;

my %anno;
my %total;
my %dids;
my $family;

print <<END_OF_TEXT;
# Sensitivity of a clustering
# FAMILY:	family that is compared
# ANNO:	        number of domains in cluster annotated by assigned reference family
# TOTAL:	number of domains in assigned reference family
# SENSI:	sensitivity
# REFF:		assigned reference family
family\tanno\ttotal\tsensi\treff
END_OF_TEXT

my $last_did;
while (<STDIN>) {
    if (/# master/) {
	print $_; 
	next;
    }

    if (/# reference/) {
	print $_; 
	next;
    }

    next if (/^\#/);
    next if (/^family/);
    chop();

    my ($did, $nunits, $nseqs, $length, 
	$aunits, $aseqs, 
	$runits, $tunits, $rseqs, $tseqs, 
	$sel, $sen, 
	$alength, $aovl, 
	$anno1, $anno2) = split(/\t/);

    $did = $last_did if (!$did);

    my $family = $anno1."\t".$anno2;
    
    if ($anno{$family} < $runits) {
	$anno{$family} = $runits;
	$total{$family} = $tunits;
	$dids{$family} = $did;
    }

    $last_did = $did;

}

my $t = 0;
my $n = 0;

for $family (sort keys %anno) {
    my $anno = $anno{$family};
    my $total = $total{$family};
    my $did = $dids{$family};
    my $r = $anno/$total;
    $t += $r; $n++;
    printf ("$did\t$anno\t$total\t%5.2f\t$family\n", $r);
}
	
print "# average sensitivity: " . $t/$n . "\n" ;


