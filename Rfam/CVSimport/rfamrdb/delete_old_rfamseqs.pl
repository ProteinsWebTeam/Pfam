#!/usr/local/bin/perl


use strict;

my $file = shift;
die "need rfamseq diff file dopey!!\n" if(!$file);

#my $file = "/pfam/db/rfamseq/6/rfamseq.diff";

my (%old_seqs);

open(_SQL, ">/pfam/db/web_temp/delete_seqs.dat");
open(_FILE, "$file");

while(<_FILE>) {
  
  next if ($_ !~ /DELETED/i);
  if ($_ =~ /(\S+)\.(\d+)\s+DELETED/) {
  #  print "$1 \n";
    $old_seqs{$1} = $1;
#    $count++;
  }

}

close(_FILE);
my $count = 0;
foreach (sort keys %old_seqs) {
  $count++;
  print _SQL "delete from rfamseq where rfamseq_acc = \"$_\";\n";
}

print "COUNT: $count \n";
 close(_SQL);
