#!/usr/local/bin/perl

#
# Checkhmmflat - checks integrity of an annotatedHMM file
#

use strict;
use Getopt::Long;

my ($version, $hmmer_version, $file);

GetOptions("f=s"   => \$file,
           "hmmer=s" => \$hmmer_version,
	         "v"     => \$version);


unless($hmmer_version){
	die "Did not get hmmer version or number of families\n";
}

unless($file){
  die "Do not know what file to process\n"; 
}

unless(-s $file){
  die "$file either does not exists or has no size\n"; 
}

open(HMM, "$file") or die "Failed to open $file:[$!]\n";

my ($namenum,$name,$acc,$desc,$build,$calibrate, $thisHmmerVersion);

while(<HMM>) {
    chomp;
	# Change when we update hmmer version	
    #if (/^HMMER2.0\s+\[${$hmmer_version[0]}[0]\]/) {
	if (/^HMMER2.0\s+\[(\S+)\]/){
	$thisHmmerVersion = $1;
	print "Hmmer version missmatch! $1 was used, expected $hmmer_version \n" if ($hmmer_version != $thisHmmerVersion);
	# start of an entry
	$namenum = $acc = $desc = $build = $calibrate = 0;
	next;
    } elsif (/^NAME  ([0-9a-zA-Z\-_]{1,15})$/){
	$name = $1;
	$namenum++;
	next;
    }  elsif (/^NAME/){
	die "Incorrectly formatted NAME field [$_]\n";
    } elsif ((/^ACC   (PF\d{5})$/) && !$version) {
	
	$acc++;
	next;
    } elsif ((/^ACC   (PF\d{5}\.\d{1,3})$/) && $version) {
	$acc++;
	next;
    } elsif (/^ACC/) {
	die "Incorrectly formatted ACC field [$_]\n";
    } elsif (/^DESC  \S/){
	$desc++;
	next;
    } elsif (/^DESC/) {
	die "Incorrectly formatted DESC line [$_]\n";
    } elsif (/^COM\s+hmmbuild/){
	$build++;
	next;
    } elsif (/^COM\s+hmmcalibrate/){
	$calibrate++;
	next;
    } elsif (/^\/\//) {
	if ($namenum != 1){
	    die "$name NAME lines for $name\n";
	}
	if ($desc != 1){
	    die "$desc DESC lines for $name\n";
	}
	if ($build != 1){
	    die "$build COM hmmbuild lines for $name\n";
	}
	if ($calibrate != 1){
	    die "$calibrate COM hmmcalibrate lines for $name\n";
	}
    } elsif (/^LENG\s+\d+/){
	next;
    } elsif (/^ALPH  Amino/){
	next;
    } elsif (/^RF    no/){
	next;
    } elsif (/^RF    yes/){
	next;
    } elsif (/^CS    no/){
	next;
    } elsif (/^CS    yes/){
	next;
	}elsif (/^MAP   yes/){
	next;
    } elsif (/^NSEQ\s+\d+/){
	next;
    } elsif (/^GA\s+[0-9.-]+\s+[0-9.-]+/){
	next;
    } elsif (/^TC\s+[0-9.-]+\s+[0-9.-]+/){
	next;
    } elsif (/^NC\s+[0-9.-]+\s+[0-9.-]+/){
	next;
    } elsif (/^CKSUM\s+\d+/){
	next;
    } elsif (/^EVD\s+\S+\s+\S+/){
	next;
    } elsif (/^NULE\s+\S+\s+\S+/){
	next;
    } elsif (/^NULT\s+\S+\s+\S+/){
	next;
    } elsif (/^XT/){
	next;
    } elsif (/^DATE/){
	next;
    } elsif (/^HMM /){
	next;
    } elsif (/^\s+/){
      next;
    }else {
      print "Unrecognised line [$_]\n";
    }
}

