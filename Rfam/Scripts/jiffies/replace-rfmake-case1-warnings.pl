use strict;

my $usage = "replace-rfmake-case1-warnings.pl <list of all SEED sequences (esl-alistat --list)> <root for output files>\n\nNOTE: rfmake.log must exist in present dir\n\n";

if(scalar(@ARGV) != 2) { die "$usage\n"; }

my ($seed_list, $outroot) = @ARGV;

open(LIST, $seed_list) || die "ERROR unable to open $seed_list"; 
my @origA = ();
while(my $line = <LIST>) { 
  chomp $line;
  push(@origA, $line);
}
my $norig = scalar(@origA);

open(LOG, "rfmake.log") || die "ERROR unable to open rfmake.log";
while(my $wline = <LOG>) { 
  if($wline =~ m/case 1/) { 
    if($wline =~ /^\! WARNING \(case 1\)\: Overlapping hit to SEED sequence exists \> GA but it\'s not an exact match \((\S+)\s+\(seed\)\s+\!\=\s+(\S+)\s+\(hit\)\)$/) { 
      my ($orig, $new) = ($1, $2);
      my $ii = -1;
      for(my $i = 0; $i < $norig; $i++) { 
        if($orig eq $origA[$i]) { 
          if($ii != -1) { die "ERROR unexpected problem finding match to $orig in original seed, $orig duplicated?"; }
          $ii = $i; 
          $origA[$i] = $new;
        }
      }
      if($ii == -1) { die "ERROR unable to find $orig in $seed_list"; }
    }
  }
}

my $sfetch_file = $outroot . ".sfetch";
open(SFETCH, ">" . $sfetch_file);
foreach my $nse (@origA) { 
  if($nse =~ m/^(\S+)\/(\d+)\-(\d+)/) { 
    my ($name, $start, $end) = ($1, $2, $3);
    print SFETCH ("$nse $start $end $name\n");
  }
  else { 
    die "ERROR unable to break down $nse into name/start-end";
  }
}
close(SFETCH);
print("esl-sfetch input file saved to $sfetch_file\n");

printf("Suggested commands:\n>esl-sfetch -Cf \$RFAMSEQ $sfetch_file > $outroot.fa\n>cmalign --cyk -g CM $outroot.fa > $outroot.stk\n");
exit 0;
