#!/usr/bin/env perl

use strict;
use warnings;
use Data::Dumper;
use File::Copy;

use Bio::Pfam::HMM::HMMResultsIO;
use Bio::Pfam::FamilyIO;
use Getopt::Long;

my ($file, $famDir, $threshold, $percentage, $help, $maxOverlaps, $maxBits); 
GetOptions( "aa=i"         => \$threshold,
  "file=s"       => \$file,
  "famDir=s"     => \$famDir,
  "h|help"       => \$help,
  "percentage=s" => \$percentage, 
  "maxOverlaps=i" => \$maxOverlaps,
  "maxBits=i"    => \$maxBits)
  or die "Failed to get options";

if($help){
  &help;
}

$maxBits = 30 unless($maxBits);

if(!$percentage or $percentage <= 0 or $percentage > 1){
  warn "Percantage overlap needs to be between 0 and 1\n";
  &help;
}

if($maxOverlaps) {
  #Check maxOverlaps is sensible
  die "The parameter maxOverlaps needs to be a number >0 and <=50" unless($maxOverlaps > 0 and $maxOverlaps <= 50);
}
else {
  $maxOverlaps=10;
}

unless(-s $file){
  die "Could not find the overlap file:".(defined($file) ? $file : "undef")."\n";
}

unless(-d $famDir){
  die "Could not find the families directory:".(defined($famDir) ? $famDir : "undef")."\n";
}

unless($threshold){
  die "Need to specify a maximum overlap threshold, set in amino acids\n";
}

#my $threshold = 120;
#my $percentage = 0.75;

my %fixes;
my %seedOverlaps;


print "#Family directory: $famDir\n";
print "#Overlap file: $file\n";
print "#Threshold: $threshold\n";
print "#Percentage: $percentage\n";
print "#Maximum number of total overlaps for families: $maxOverlaps\n";
print "#Maximum bits: $maxBits\n";

#Read overlaps file and store SEED overlaps (these seq need to stay with family)
my %family_overlaps;
open(O, $file) or die "Could not open $file:[$!]\n";
while(<O>){
  my($seq1, $seq2, $s1, $s2, $e1, $e2, $l1, $l2, $fam1, $fam2, $ali1, $ali2, $index, $bits);
  if(/(\(\d{1}\)) .* (PF\d+) (ALIGN|SEED) \: (\S+)\/(\d+)\-(\d+) \((\S+) bits\) .* (PF\d+) (ALIGN|SEED) (\S+)\/(\d+)\-(\d+)/){
    $fam1 = $2;
    $ali1 = $3;
    $seq1 =  $4;
    $s1 = $5;
    $e1 = $6;
    $bits = $7;
    $fam2 = $8;
    $ali2 = $9;
    $seq2 = $10;
    $s2 = $11;
    $e2 = $12;
    $index = $1;

    $family_overlaps{$fam1}++;
    $family_overlaps{$fam2}++;
  }else{
    warn "Did not parse [$_]\n"; 
  }

  next if($index eq '(3)');
  next if($ali1 eq 'ALIGN' and $ali2 eq 'ALIGN'); 

  if(defined($ali2) and  $ali2 eq 'SEED'){
    #This sequence has to stay with this family
    $seedOverlaps{$seq2} = $fam2;
  }elsif(defined($ali1) and $ali1 eq 'SEED'){
    #This sequence has to stay with this family
    $seedOverlaps{$seq1} = $fam1;
  }
}
close(O);

#print Dumper(%seedOverlaps);

open(NOT, ">notResolved.$$.txt") or die "Couldn't open fh to notResolved.$$.txt, $!";
my $canResolve=0;
open(O, $file) or die "Could not open $file:[$!]\n";
while(<O>){
  my($seq1, $seq2, $s1, $s2, $e1, $e2, $l1, $l2, $fam1, $fam2, $ali1, $ali2, $index, $bits1, $bits2);
  if(/(\([1|2|3]\)) .* (PF\d+) (ALIGN|SEED) \: (\S+)\/(\d+)\-(\d+) \((\S+) bits\) .* (PF\d+) (ALIGN|SEED) (\S+)\/(\d+)\-(\d+) \((\S+) bits\)/){
    $fam1 = $2;
    $ali1 = $3;
    $seq1 =  $4;
    $s1 = $5;
    $e1 = $6;
    $bits1 = $7;
    $fam2 = $8;
    $ali2 = $9;
    $seq2 = $10;
    $s2 = $11;
    $e2 = $12;
    $bits2 = $13;
    $index = $1;
  }else{
    die "Bad line: $_";
    next;
  }

  next if($maxBits and $bits1 > $maxBits and $bits2 > $maxBits);
  next if( ($family_overlaps{$fam1} > $maxOverlaps) or ($family_overlaps{$fam2} > $maxOverlaps) );


  next if($ali1 eq "SEED" and $ali2 eq "SEED");
  $l2 = $e2-$s2+1;
  $l1 = $e1-$s1+1;

  if($s2 <= $s1 and $e2 >= $s1) { #Type 1 overlap

    if($s1 >= $s2 and $e1 <= $e2) { #Need to remove whole of domain1
      my $o = $e1 - $s1 +1;
      print "$seq1, $s1, $e1, $seq2, $s2, $e2, overlap is: $o\n";

      if($o < $threshold and ($o/$l1 <= $percentage) and $o/$l2  <= $percentage){

        if($seedOverlaps{$seq1} and $seedOverlaps{$seq1} eq $fam1){
          print NOT $_;
          print NOT "Can't resolve (seed overlap), skipping\n";
        }else{
          print "I can fix this overlap $fam1:$seq1/$s1-$e1 $fam2:$seq2/$s2-$e2\n";
          $canResolve++;
          push(@{ $fixes{$fam1} }, { seq   => $seq1,
              from  => $s1,
              to    => $e1,
              eFrom => "",
              eTo   => "" });
          print "$fam1 ($bits1 bits), $seq1/$s1-$e1 => remove\n";

        }
      }
      else {
        print NOT $_;
        print NOT "Can't resolve (big overlap:1), skipping (overlap = $o, domain 1 = $l1,  domain 2 = $l2)\n";
      }
    }
    else { #Need to remove part of domain1
      my $o = $e2 - $s1; +1; 
      print "$seq1, $s1, $e1, $seq2, $s2, $e2, overlap is: $o\n";

      if($o < $threshold and ($o/$l1 <= $percentage) and $o/$l2  <= $percentage){

        if($seedOverlaps{$seq1} and $seedOverlaps{$seq1} eq $fam1){
          if($bits2 > $maxBits) {
            print NOT $_;
            print NOT "Can't resolve (seed overlap with high bit score), skipping\n";
          }
          else {
            print "I can fix this overlap $fam1:$seq1/$s1-$e1 $fam2:$seq2/$s2-$e2\n";
            $canResolve++;
            push(@{ $fixes{$fam2} }, {  seq   => $seq2,
                from  => $s2,
                to    => $e2,
                eFrom => $s2,
                eTo   => $s1-1 });
            print "$fam2 ($bits2 bits), $seq2/$s2-$e2 => $seq2/$s2-($s1-1)\n";
          }
        }else{
          print "I can fix this overlap $fam1:$seq1/$s1-$e1 $fam2:$seq2/$s2-$e2\n";
          $canResolve++;
          push(@{ $fixes{$fam1} }, { seq   => $seq1,
              from  => $s1,
              to    => $e1,
              eFrom => $e2+1,
              eTo   => $e1 });
          print "$fam1 ($bits1 bits), $seq1/$s1-$e1 => $seq1/($e2+1)-$e1\n";
        }
      }
      else{ 
        print NOT $_;
        print NOT "Can't resolve (big overlap:1), skipping (overlap = $o, domain 1 = $l1,  domain 2 = $l2)\n";
      }
    }
  }
  elsif($e1 >= $s2 and $e1 <= $e2)  { #Type 2 overlap
    my $o = $e1 - $s2 +1;
    print "$seq1, $s1, $e1, $seq2, $s2, $e2, overlap is: $o\n";

    if($o < $threshold and ($o/$l1 <= $percentage) and $o/$l2  <= $percentage){

      if($seedOverlaps{$seq1} and $seedOverlaps{$seq1} eq $fam1){

        if($bits2 > $maxBits) {
          print NOT $_;
          print NOT "Can't resolve (seed overlap with high bit score), skipping\n";
        }
        else {
          print "I can fix this overlap $fam1:$seq1/$s1-$e1 $fam2:$seq2/$s2-$e2\n";
          $canResolve++;
          push(@{ $fixes{$fam2} }, {  seq   => $seq2,
              from  => $s2,
              to    => $e2,
              eFrom => $e1+1,
              eTo   => $e2 });
          print "$fam2 ($bits2 bits), $seq2/$s2-$e2 => $seq2/($e1+1)-$e2\n";
        }
      }else{
        print "I can fix this overlap $fam1:$seq1/$s1-$e1 $fam2:$seq2/$s2-$e2\n";
        $canResolve++;
        push(@{ $fixes{$fam1} }, { seq   => $seq1,
            from  => $s1,
            to    => $e1,
            eFrom => $s1,
            eTo   => $s2-1 });
        print "$fam1 ($bits1 bits), $seq1/$s1-$e1 => $seq1/$s1-($s2-1)\n";

      }
    }
    else{ 
      print NOT $_;
      print NOT "Can't resolve (big overlap:2), skipping (overlap = $o, domain 1 = $l1,  domain 2 = $l2)\n";
    } 
  }
  else { #Type 3 overlap
    print NOT "Can't resolve type 3 overlap:  $fam1:$seq1/$s1-$e1 ($bits1 bits) $fam2:$seq2/$s2-$e2 ($bits2 bits)\n";
  }
}
close NOT;
print STDERR "$canResolve overlaps can be resolved\n";

my $noOverlaps;
open(E, ">edits.$$.txt") or die "Could not open edits:[$!]\n";
foreach my $fam (sort{ $fixes{$b} <=> $fixes{$a} }keys %fixes){
  if(@{$fixes{$fam}} < 50){
    print "=== $fam ===\n";
    print E "=== FAMILY: $fam ===\n";
    chdir("$famDir/$fam") or die "Could not change directory to $famDir/$fam\n";
    my $hmmio  = Bio::Pfam::HMM::HMMResultsIO->new;
    my $hmmres = Bio::Pfam::HMM::HMMResultsIO->parsePFAMOUT('PFAMOUT');
    my $familyIO = Bio::Pfam::FamilyIO->new;

    my $desc  = $familyIO->parseDESC('DESC');
    copy('DESC', 'DESC.beforeEdit');

    unless ( $desc->EDITS ){
      $desc->EDITS( [] ); 
    }

    foreach my $edit (@{$fixes{$fam}}){
      foreach my $u (@{ $hmmres->units}){
        if($u->name eq $edit->{seq} and 
          $u->seqFrom <= $edit->{from} and $u->seqTo > $edit->{from}
            and $u->seqTo >= $edit->{to} and $u->seqFrom <= $edit->{to} ){

          my $newFrom = ($edit->{eFrom} eq $edit->{from} ? $u->envFrom : $edit->{eFrom});
          my $newTo   = ($edit->{eTo} eq $edit->{to} ? $u->envTo : $edit->{eTo} ) ;
          print "newFrom = $newFrom; newTo = $newTo\n";
          my $newEdit = 1;
          foreach my $oldEdit (@{$desc->EDITS}){
            if($oldEdit->{seq} eq $edit->{seq} and 
              $oldEdit->{oldFrom} == $u->envFrom and 
              $oldEdit->{oldTo} == $u->envTo ) {
              #Looks like we already have this edit! 
              if($newTo < $oldEdit->{oldTo}){
                $oldEdit->{newTo} = $newTo;
              }
              if($newFrom > $oldEdit->{oldFrom}){
                $oldEdit->{newFrom} = $newFrom;
              }
              $newEdit = 0;
              last;     
            }
          }


          push(@{$desc->EDITS}, { seq     => $edit->{seq}, 
              oldFrom => $u->envFrom, 
              oldTo   => $u->envTo, 
              newFrom => $newFrom, 
              newTo   => $newTo}) if ($newEdit);


          print E "ED   ".$edit->{seq}."/".$u->envFrom."-".$u->envTo."; ";
          $noOverlaps++;

          print E $edit->{seq}."/".($edit->{eFrom} == $edit->{from} ? $u->envFrom : $edit->{eFrom}).
          "-".($edit->{eTo} == $edit->{to} ? $u->envTo : $edit->{eTo} ).";\n";
          last;
        }
      }
    }
    $familyIO->writeDESC($desc);
    print STDERR "Doing pfmake on $fam\n";
    system('pfmake.pl');
    chdir($famDir) or die "Couldn't chdir back to $famDir, $!";
  }
}
print "Resolved $noOverlaps\n";

sub help {

  print <<EOF;

  !!!WARNING - this is a very dangerous script to run.....

  $0 <options>

  All options are mandatory, apart from maxOverlaps:

  aa          : An amino acid size limit of the maximum number of amino acids that it allowed to be removed. (e.g. 100)
  percentage  : The maximum percentage of the domain length that can be wiped out by and ED line. Must
                be a number between 0 and 1. (e.g. 0.60).
  file        : File containing the list of overlaps.
  famDir      : The directory containing the families.
  maxOverlaps : Only work on families that have less than or equal to this number of overlaps (default 10)

EOF

  exit;

}
