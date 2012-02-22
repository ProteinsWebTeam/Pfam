#!/usr/local/bin/perl
#

use strict;
use warnings;
use Data::Dumper;
use File::Copy;

use Bio::Pfam::HMM::HMMResultsIO;
use Bio::Pfam::FamilyIO;
use Getopt::Long;

my ($file, $famDir, $threshold, $percentage, $help); 
GetOptions( "aa=i"         => \$threshold,
            "file=s"       => \$file,
            "famDir=s"     => \$famDir,
            "h|help"       => \$help,
            "percentage=s" => \$percentage)            
or die "Failed to get options";

if($help){
  &help;
}


if(!$percentage or $percentage <= 0 or $percentage >= 1){
  warn "Percantage overlap needs to be between 0 and 1\n";
  &help;
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

open(O, $file) or die "Could not open $file:[$!]\n";
while(<O>){
  my($seq1, $seq2, $s1, $s2, $e1, $e2, $l1, $l2, $fam1, $fam2, $ali1, $ali2, $index);
  if(/(\(\d{1}\)) .* (PF\d+) (ALIGN|SEED) \: (\S+)\/(\d+)\-(\d+) .* (PF\d+) (ALIGN|SEED) (\S+)\/(\d+)\-(\d+)/){
    $fam1 = $2;
    $ali1 = $3;
    $seq1 =  $4;
    $s1 = $5;
    $e1 = $6;
    $fam2 = $7;
    $ali2 = $8;
    $seq2 = $9;
    $s2 = $10;
    $e2 = $11;
    $index = $1;
  }else{
    warn "Did not parse $_\n"; 
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

open(O, $file) or die "Could not open $file:[$!]\n";
while(<O>){
  my($seq1, $seq2, $s1, $s2, $e1, $e2, $l1, $l2, $fam1, $fam2, $ali1, $ali2, $index);
  if(/(\([2|3]\)) .* (PF\d+) (ALIGN|SEED) \: (\S+)\/(\d+)\-(\d+) .* (PF\d+) (ALIGN|SEED) (\S+)\/(\d+)\-(\d+)/){
    $fam1 = $2;
    $ali1 = $3;
    $seq1 =  $4;
    $s1 = $5;
    $e1 = $6;
    $fam2 = $7;
    $ali2 = $8;
    $seq2 = $9;
    $s2 = $10;
    $e2 = $11;
    $index = $1;
  }elsif(/(\(1\)) .* (PF\d+) (ALIGN|SEED) \: (\S+)\/(\d+)\-(\d+) .* (PF\d+) (ALIGN|SEED) (\S+)\/(\d+)\-(\d+)/){
    $fam1 = $7;
    $ali1 = $8;
    $seq1 = $9;
    $s1   = $10;
    $e1   = $11;
    $fam2 = $2;
    $ali2 = $3;
    $seq2 = $4;
    $s2   = $5;
    $e2   = $6;
    $index = $1;
  }else{
    die;
    next;
  }
 

  next if($ali1 eq "SEED" and $ali2 eq "SEED");
  $l2 = $e2-$s2+1;
  $l1 = $e1-$s1+1;
  if($s1<= $s2 and $e1 <= $e2){ 
   my $o = $e1 - $s2 +1;
   print "$seq1, $s1, $e1, $seq2, $s2, $e2\n";
   print "Overlap is: $o\n";
   if($o < $threshold and ($o/$l1 < $percentage) and $o/$l2  < $percentage){
      print "I can fix this overlap\n";
      if($seedOverlaps{$seq1} and $seedOverlaps{$seq1} eq $fam2){
        push(@{ $fixes{$fam1} }, {  seq   => $seq1,
                                    from  => $s1,
                                    to    => $e1,
                                    eFrom => $s1,
                                    eTo   => $s2-1 });
      }else{
         push(@{ $fixes{$fam2} }, { seq   => $seq2,
                                    from  => $s2,
                                    to    => $e2,
                                    eFrom => $e1+1,
                                    eTo   => $e2 });
       
     }     
   }else{ 
     print "Big overlap, skipping (overlap = $o, domain 1 = $l1,  domain 2 = $l2)\n";
   }
  }
}

my $noOverlaps;
open(E, ">edits.txt") or die "Could not open edits:[$!]\n";
foreach my $fam (sort{ $fixes{$b} <=> $fixes{$a} }keys %fixes){
  if(@{$fixes{$fam}} < 50){
    print STDERR "=== $fam ===\n";
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
              
              my $newFrom = ($edit->{eFrom} == $edit->{from} ? $u->envFrom : $edit->{eFrom});
              my $newTo   = ($edit->{eTo} == $edit->{to} ? $u->envTo : $edit->{eTo} ) ;
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
    system('pfmake.pl');
  }
}
print "Resolved $noOverlaps\n";

sub help {
  
print <<EOF;
  
  !!!WARNING - this is a very dangerous script to run.....

  $0 <options>

  All options are mandatory:

  aa         : An amino acid size limit of the maximum number of amino acids that it allowed to be removed. (e.g. 100)
  percentage : The maximum percentage of the domain length that can be wiped out by and ED line. Must
               be a number between 0 and 1. (e.g. 0.60).
  file       : File containing the list of overlaps.
  famDir     : The directory containing the families.

EOF

exit;

}
