#!/usr/local/bin/perl

use strict;


my $dir = shift; ## Dir with all the embl files

open(_EMBL, ">embl.dat");

opendir(_WHOLE, "$dir") || die("Could not open $dir $!");

foreach my $temp_file ( readdir(_WHOLE) ) {
  $temp_file =~ /^\.+$/ && next;
  next if ($temp_file !~ /\.dat$/);
  print "file: $temp_file \n";

  open(_FILE, "$dir/$temp_file");

  my($id, $acc, $desc, $os, $oc, $version, $prev);

  while(<_FILE>) {
   # print "$_ ";sleep 1;
    chop($_);
    if (/\ID\s+(\S+)/){
      print _EMBL "$id~$acc~$version~$desc~$os~$oc~$prev\n" if ($acc);
      $id = $acc = $desc = $os = $oc = $version = $prev = undef;
      
      
      $id = $1;
     #  print "ID: $id \n";
    }

    if (/^AC(.*)/){
    #  print "ACC: $1 ";
      $acc .= $1;
      chop($acc);
      if ($acc =~ /;/) {
#	print "ACC: $acc \n";
	my @junk;
	($acc, @junk) = split(/;/, $acc);
	$prev = join ';', @junk;
#	print "PREV: $prev \n\n";
	# print "AC: $acc \n" if ($acc =~ /;/); 
      }


    }

    if (/\SV\s+(\S+)/){
      $version = $1;
      #print "version: $version \n";
      #if ($acc =~ /;/) {
#	print "ACC: $acc :: version: $version \n";
#      }
    }


    if (/\DE\s+(.*)/){
      $desc .= " " . $1;
      #print "description: $desc \n";
    }
    
    if (/\OS\s+(.*)/){
      $os .=  $1;
     # print "species: $os \n";
    }


    if (/\OC\s+(.*)/){
      $oc .= $1;
     # print "tax: $oc \n";
    }

  }

  close(_FILE);

}

close(_WHOLE);
close(_EMBL);


