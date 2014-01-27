#! /usr/bin/env perl

# A script to remove overlapping sequences from an alignment

my $align=shift @ARGV;
my $overlap=shift @ARGV;;


#Check input files exist
unless($align and $overlap) {
  help();
}
if (! -e $overlap or ! -s $align){
  print "Check overlap and align files exist\n\n";
  help();
}

#Read overlap data
my %overlap;
open (OVERLAP, $overlap) or die "Cannot open $overlap file, $!";
while(<OVERLAP>){
  if (/\[(\S+)\].*SEED with/){
    $overlap{$1}=1;
  } 
  elsif (/\[(\S+)\].*JALIGN with/){
    $overlap{$1}=1;
  } 
  elsif (/\[(\S+)\].*FULL with/){
    $overlap{$1}=1;
  } 
  else {
    warn "Unrecognized line in overlap file: $_";
  }
}
close OVERLAP;

#Go through align file and print all ids that do not appear in overlap file
open (ALIGN, $align) or die "Cannot open $align file, $!";
while(<ALIGN>){
  if (/^(\S+).\d+\/\d+-\d+\s+\S+$/){
    my $name=$1;
    
    next if(exists($overlap{$name})); #Don't print if $name is in overlap file
    print $_;
    
  } 
  elsif(/^\/\//) { #Don't print // at end of alignment
  }
  else {
    warn "Unrecognized line: $_";
  }
}
close ALIGN;


sub help {
    print STDERR <<"EOF";
Usage: $0 <align file> <overlap file>

This program will take a stockholm/mul format alignment and 
overlap file and remove any overlapping alignment segments 
and print the result to STDOUT. 

EOF
exit 0;
}
 
