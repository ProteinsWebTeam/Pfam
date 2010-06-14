#!/usr/local/bin/perl
#
# Script to automagically resolve overlaps by adding ED lines

use strict;
use warnings;
use Data::Dumper;
use File::Copy;
use Getopt::Long;

use Bio::Pfam::HMM::HMMResultsIO;
use Bio::Pfam::FamilyIO;

my ($help,$overlap_file,$family_dir,$threshold,$percentage);
&GetOptions('help!'         => \$help,
            'overlap=s'     => \$overlap_file,
	    'family_dir=s'  => \$family_dir,
            'threshold=s'   => \$threshold,
            'max_overlap=s' => \$percentage,
	    );

if (! $overlap_file or $family_dir){$help=1}

if ($help){
    print STDERR <<"EOF";
This script is used during the overlap resolution stage of the Pfam sequence
update.

Usage: $0 

Required options:
  -overlap <overlap file> 
  -family_dir <family dirs>

Optional options
  -threshold <max length of an ED line edit>
  -max_overlap <max fraction of either region that can be edited>
  -help


EOF
    exit 0;
}


# If no values are given choose relatively conservative values
if (! $threshold){$threshold = 10;}
if (! $percentage){$percentage = 0.25;}


my %fixes;
my %seedOverlaps;

open(O, $overlap_file) or die "Could not open file $overlap_file:[$!]\n";
while(<O>){
  my($seq1, $seq2, $s1, $s2, $e1, $e2, $l1, $l2, $fam1, $fam2, $ali1, $ali2, $index);
  if(/(\(2\)) .* (PF\d+) (ALIGN|SEED) \: (\S+)\/(\d+)\-(\d+) .* (PF\d+) (ALIGN|SEED) (\S+)\/(\d+)\-(\d+)/){
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
    warn "Did not parse $_\n"; 
  }
  next if($ali1 eq 'ALIGN' and $ali2 eq 'ALIGN'); 

  if($ali2 eq 'SEED'){
   #This sequence has to stay with this family
   $seedOverlaps{$seq2} = $fam2;
  }elsif($ali1 eq 'SEED'){
    #This sequence has to stay with this family
    $seedOverlaps{$seq1} = $fam1;
  }
}
close(O);

print Dumper(%seedOverlaps);

open(O, $overlap_file) or die "Could not open $overlap_file:[$!]\n";
while(<O>){
  my($seq1, $seq2, $s1, $s2, $e1, $e2, $l1, $l2, $fam1, $fam2, $ali1, $ali2, $index);
  if(/(\(2\)) .* (PF\d+) (ALIGN|SEED) \: (\S+)\/(\d+)\-(\d+) .* (PF\d+) (ALIGN|SEED) (\S+)\/(\d+)\-(\d+)/){
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
    next;
  }
  
  $l2 = $e2-$s2+1;
  $l1 = $e1-$s1+1;
  if($s1<= $s2 and $e1 <= $e2){ 
   my $o = $e1 - $s2 +1;
   print "$seq1, $s1, $e1, $seq2, $s2, $e2\n";
   print "Overlap is: $o\n";
   if($o < $threshold and ($o/$l1 < $percentage) and $o/$l2  < $percentage){
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
     print "Big overlap\n";
   }
  }
}

my $noOverlaps;
open(E, ">edits.txt") or die "Could not open edits:[$!]\n";
foreach my $fam (sort{ $fixes{$b} <=> $fixes{$a} }keys %fixes){
  if(@{$fixes{$fam}} < 50){
    print "=== $fam ===\n";
    print E "=== FAMILY: $fam ===\n";
    chdir("$family_dir/$fam") or die "Could not change directory to $family_dir/$fam\n";
    my $hmmio  = Bio::Pfam::HMM::HMMResultsIO->new;
    my $hmmres = Bio::Pfam::HMM::HMMResultsIO->parsePFAMOUT('PFAMOUT');
    my $familyIO = Bio::Pfam::FamilyIO->new;
    
    my $desc  = Bio::Pfam::FamilyIO->parseDESC('DESC');
    copy('DESC', 'DESC.beforeEditGo3');
 
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
