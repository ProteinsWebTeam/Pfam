## Bioperl Test Harness Script for Modules

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.t'

#-----------------------------------------------------------------------
## perl test harness expects the following output syntax only!
## 1..3
## ok 1  [not ok 1 (if test fails)]
## 2..3
## ok 2  [not ok 2 (if test fails)]
## 3..3
## ok 3  [not ok 3 (if test fails)]
##
## etc. etc. etc. (continue on for each tested function in the .t file)
#-----------------------------------------------------------------------


## We start with some black magic to print on failure.
BEGIN { $| = 1; print "1..2\n"; }
END {print "not ok 1\n" unless $loaded;}

use lib '/nfs/disk100/pubseq/Pfam/scripts/pfamqc';
use Bio::Pfam;
use PfamQC;

$loaded = 1;
print "ok 1\n";    # 1st test passes.


## End of black magic.
##
## Insert additional test code below but remember to change
## the print "1..x\n" in the BEGIN block to reflect the
## total number of tests that will be run. 



# test 2

my $current = "/nfs/disk100/pubseq/Pfam/CURRENT";
my @famlist = glob "$current/*/";
chop @famlist;
my $overlaps = &PfamQC::local_dirs_overlap( \@famlist );
if( $overlaps ) {
    print "not ok 2\n";
}
else {
    print "ok 2\n";
}


