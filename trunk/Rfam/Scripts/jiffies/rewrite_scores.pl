#!perl
#
# seq_acc/start-end  start  end  seq_acc  bit_score  evalue  CM_start  CM_end  Partial  type(seed/full)
# \_take_from scores.evalue________________________________/ \_dummy_values___________________________/
#                                                                1         100     no         seed
# run something like:
#
# shell% for file in `find ./100 -name scores.evalue -print`; do perl rewrite_scores.pl $file ${file%%evalue}new; done

use strict;
use warnings;

my ( $in, $out ) = @ARGV;
die "ERROR: no input file ($in): $!"
  unless -e $in;
die "ERROR: output file exists ($out)"
  if -e $out;

open ( IN, $in )
  or die "ERROR: couldn't open input file for read: $!";

open ( OUT, ">$out" )
  or die "ERROR: couldn't open output file for write: $!";

print "$in... ";
while ( <IN> ) {
  my ( $score, $evalue, $seq_acc, $start, $end ) = 
    m|^(\S+)\s+(\S+)\s+([A-Z0-9\.]+)/(\d+)\-(\d+)|;
  print OUT "$seq_acc/$start-$end $start $end $seq_acc $score $evalue 1 100 no seed\n";
}
print "done\n";

