#! /software/bin/perl

use strict;

my $dir = shift;

opendir(_EX_DIR, "$dir") || die("Could not open $dir $!");

## delete all the files in temp dir
foreach my $temp_file ( readdir(_EX_DIR) ) {
  $temp_file =~ /^\.+$/ && next;
  
#  $temp_file = "RF00163.species";

  open(_OUT, ">/nfs/WWWdev/SANGER_docs/htdocs/Software/Rfam/data/species/" . "$temp_file");
  
  open(_FILE, "$dir/" . "$temp_file");
  
 # print "FILE: $temp_file ";
  my $fix = 0;
  while(<_FILE>) {
      print STDERR "processing $temp_file\n";
    
    if ($_ !~ /;/) {
      print _OUT $_;
      next;
    }
    #print "$_";
    chop($_);
    my($acc, $spec, $tax) = split(/~/, $_);
    my ($start_spec) = $1 if ($spec =~ /^(\S+)\s+/);
   # print "start: $start_spec ($spec)\n";
    my(@tax) = split(/;/, $tax);
    my $last;
    my $last_count = 0;
    foreach (@tax) {
      $last = $_;
      $last_count++;
    }
    $last =~ s/^\s+//;

    if ($spec =~ /$last/) {

      #print "equals: $spec -> $tax\n";
      my($start) = $1 if ($last =~ /(\S+)/);
    
      my @new_tax;
      
      if ($start !~ /\./) {
	foreach (@tax) {
	  my($first_tmp) =  $1 if ($_ =~ /(\S+)/);
	  $first_tmp =~ s/\s+//g;
	  $start  =~ s/\s+//g;
	  
	  if ($first_tmp eq $start) {
	    next;
	  } else {
	    push @new_tax, $_;
	  }
	}
	#print "start: $start \n";
	if ($start eq $start_spec) {
	  #print "equals\n";
	} else {
	  #print "$acc ::: dont match $tax($start) & $start_spec ($spec)\n";
	}
	push @new_tax, $start . ".";
      } else {
	@new_tax = @tax;
      }
      
      my $joined_tax = join ";",  @new_tax;
      
      print _OUT $acc . "~" . $spec . "~" .$joined_tax . "\n";



     # print _OUT $acc . "~" . $spec . "~" .$tax . "\n";
    } else {
      $fix = 1;
     # $tax =~ s/\.$/\;/;
     # $tax = $tax . $spec . ".";
      #print "dont equal $spec =~ $last \n";
      my $spec_start = $1 if ($spec =~ /^(\S+)\s+/);
     # print "spec_start: $spec_start \n";
      #print "
      $tax =~ s/\./\;$spec_start\./;
#      print $acc . "~" . $spec . "~" .$tax . "\n";
      print _OUT $acc . "~" . $spec . "~" .$tax . "\n";
    }

##    next;


    
    
    
  }
  
  #print "$fix \n";
 # print "$temp_file EEP FIX\n" if ($fix > 0);
  close(_FILE);
  close(_OUT);
#  exit(0);
}
