#!/usr/bin/env perl

# Code to trim a pfam alignment down to an acceptable level of gaps at N- and C- terminus...

use strict;
use warnings;
use Getopt::Long;

use Bio::Easel::MSA;

my($threshold, $terminal, $input, $output, $help);
$threshold = 0.7;
$terminal = 'both';

GetOptions( "in=s"         => \$input,
            "out=s"        => \$output,
            "threshold=s"   => \$threshold,
            "terminal=s"   => \$terminal,
            "help|h"       => \$help) or die "Invalid option! See $0 -h\n";


help() if($help);
if(!$input){
  warn "No input file specified\n";
  $help = 1;  
}

if(!$output){
  warn "No output file specified\n";
  $help = 1
}

if($threshold > 1 or $threshold < 0){
  warn "$threshold expected to be between 0 and 1, default 0.5\n"; 
  $help = 1;
}

if($terminal ne 'N' and $terminal ne 'C' and $terminal ne 'both'){
  warn "terminal options should be one of N, C or both. Default [both].\n";
  $help = 1;
}

help() if($help);



my $msa = Bio::Easel::MSA->new( { 'fileLocation' => $input } );

#initiate an array to tell which columns need to be removed;
my @apos;
for (my $pos = 0; $pos < $msa->alen; $pos++){
  $apos[$pos]=1;
}


my $noSeqs = $msa->nseq;

#Detemine amount to trim from the N-terminal
if($terminal ne 'C'){
  for (my $pos = 1; $pos <= $msa->alen; $pos++){
    my $occupancy = colOccupancy($msa->get_column($pos), $noSeqs);
    if($occupancy >= $threshold){
      last; #Reached threshold, do not walk through rest of alignment
    }else{
      $apos[$pos - 1] = 0;
    }
  }
}

#Trim the C-terminal
if($terminal ne 'N'){
  for (my $pos = $msa->alen; $pos >= 1; $pos--){
    my $occupancy = colOccupancy($msa->get_column($pos), $noSeqs);
    if($occupancy >= $threshold){
      last;
    }else{
      $apos[$pos - 1] = 0;
    }
  } 
}


#Now remove any columns and write the alignment.
$msa->column_subset_rename_nse(\@apos, 1);
$msa->write_msa( $output, 'pfam');


sub colOccupancy {
  my ($col, $noSeqs) = @_;
  my $noChars = () = $col =~ /[A-Z]/gi;
  $noSeqs = length($col) if(!$noSeqs); 
  my $o = $noChars/$noSeqs; 
  return $o;
}

sub help {
  print<<EOF;

usage: $0 -in my.ali -out my_trimmed.ali

Options
-------

  threshold : number between 0 and 1, default is 0.7
            : i.e. 70% of sequences have to be aligned

  terminal  : N, C or both
            : controls which end of the alignment will be trimmed

  in        : input filename

  out       : output filename

  h|help    : prints this message

EOF

exit;
}
