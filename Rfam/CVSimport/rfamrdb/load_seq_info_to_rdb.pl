#!/usr/local/bin/perl 

use lib '/nfs/WWWdev/SANGER_docs/cgi-bin/Rfam';

use RfamWWWConfig;
use EntryWWW;

use lib '/nfs/disk56/mm1/rfam/scripts/Modules';

require "/nfs/disk56/mm1/rfam/scripts/Modules/UpdateRDB.pm";
require "/nfs/disk56/mm1/rfam/scripts/Modules/Bio/Rfam.pm";

#use UpdateRDB;
#use Rfam;

use strict;

open(_UPDATE, ">seed_upload_rfamseq.dat");
open(_DATA, "all_rfamseq.dat");

my($acc, $os, $oc, $de, $id);
my %acc;
my $prev_acc = 0;
my $got_oc  = 0;

while(<_DATA>) {
 # if ( ($_ =~ /^AC\s+(\S+);/) && (!$prev_acc)  ){
  if  ($_ =~ /^AC\s+(\S+);/) {
    $prev_acc = 1;
  #  print "ACC: $1 \n";
    $acc{$1} = $1;
   print _UPDATE "$acc~$id~$de~$os~$oc\n";
    $acc = $1;
    $de = $os = $oc = $id = undef;
    $got_oc = 0;
  }

  if ($_ =~ /^DE\s+(.*)/) {
    $prev_acc = 0;
    $de .= $1;
  }

   if ($_ =~ /^ID\s+(\S+)/) {
     $id = $1;
  }


  if ($_ =~ /^OS\s+(.*)/) {
    if ($oc) {
      print "$acc: multi OS \n";
      $got_oc = 1;
      next;
    } else {
 
      $prev_acc = 0;
      $os .= $1;
    }

 }
  
  if ( ($_ =~ /^OC\s+(.*)/) && (!$got_oc) ) {
    $prev_acc = 0;
    $oc .= $1;
  }
  

}

close(_DATA);
close(_UPDATE);

my $count = 0;
foreach (sort keys %acc) {
  $count++;
}

print "COUNT: $count \n";
