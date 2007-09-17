#! /software/bin/perl

# A script to enter an i-spell session for the free text of a family

# Make a file of free text only. Keep line numbers for eachline.
# Free text lines will be those beginning with RT or CC.

use strict;
use Rfam;

#my ($dictionary) = "$Rfam::rfam_root_dir/DICTIONARY/dictionary";
#temp us the pfam one..this needs fixed.
my ($dictionary) = "/lustre/pfam/pfam/Production/Pfam/DICTIONARY/dictionary";

if( $#ARGV == -1 ) {
    print "rqc-spell.pl. Checks spelling of free text in DESC files.\nUsage rqc-spell.pl <rfam-directories>\n";
    exit(1);
}

my ($debug)=0;    # Set to true to get meagre debugging output

my ($family);
foreach $family (@ARGV) {
  my (%line);
  print "Spell checking family $family\n";

  my ($line) = 0;
  open (DESC, "$family/DESC")||die "Can't open DESC file for family $family\n";
  while (<DESC>){
    # If a free text line add to %lines
    if (/^RT   (\.*)$/){
      $line{"$line"}=$1;
    } elsif (/^CC   (.*)$/){
      $line{"$line"}=$1;
    } elsif (/^RC   (.*)$/){
      $line{"$line"}=$1;
    } elsif (/^DC   (.*)$/){
      $line{"$line"}=$1;
    } elsif (/^DE   (.*)$/){
      $line{"$line"}=$1;
    }
    $line++;
  }
  close (DESC);

  my ($bit,@line_number_array);
  # Now make temporary file
  open (TMP, "> tmp.$$")||die "Can't write to temp file\n";
  foreach $bit (sort numerically keys %line){
    # Make an array to store line numbers
    push(@line_number_array,$bit);
    print TMP $line{"$bit"},"\n";
  }
  close TMP;

  # Start ispell session on file
  system ("ispell -W 0 -w 0123456789 -p$dictionary tmp.$$");

  # Now need to put changes back into DESC file
  my (%editedline,$line_number);
  open (TMP, "tmp.$$")||die "Can't open temp file tmp.$$\n";
  while (<TMP>){
    if (/^(.*)$/){
      $line_number=shift @line_number_array;
      $editedline{"$line_number"}=$1;
    } else {
      die "unrecognised line [$_]\n Serious error!\n";
    }
  }
  close (TMP);

  # Write out new DESC file
  open (TEMPDESC, "> $family/DESC.$$")||die "Can't write to temp DESC file for family $family\n";

  open (DESC, "$family/DESC")||die "Can't open DESC file for family $family\n";
  my ($prefix);
  $line=0;

  if ($debug){
    print STDOUT "PRINTING TO DESC.$$\n";
  }
  while (<DESC>){
    if ($editedline{"$line"}){
      # Find if DE, RT or CC line
      if ($_ =~ /^(\S+)/){
	$prefix=$1;
      } else {
	die "unrecognised line [$_]\n";
      }
      # Write out line
      if ($debug){
	print STDOUT "$prefix   $editedline{$line}\n";
      }
      print TEMPDESC "$prefix   $editedline{$line}\n";
    } else {
      if ($debug){
	print STDOUT;
      }
      print TEMPDESC;
    }
    $line++;
  }
  close (DESC);
  close (TEMPDESC);

  # Move DESC across
  system ("cp $family/DESC.$$ $family/DESC");

  # Add spell file to directory so programs can enforce spell check.
  system ("touch $family/spell");


  # Clean up
  unlink ("tmp.$$");
  unlink ("tmp.$$.bak");
  if ($debug){
    sleep 2;  # Stops I-spell session removing Debugging info
  }
}



#____________________________________________________________________
# Title    : numerically
# Function : 
# Usage    : sort numerically (@array);
#-------------------------------------------------------------------
sub numerically{  $a <=> $b;  }

