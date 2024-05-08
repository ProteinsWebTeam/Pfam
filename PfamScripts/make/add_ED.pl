#!/usr/bin/env perl

# Script to take overlap file in Pfam directory and generate ED lines
# for each line of overlap file. Should be run from within Pfma family dir.

use strict;
use warnings;

open (OVERLAP, "overlap") or die "Cannot open overlap file in this directory";
open (DESC, ">> DESC") or die "Cannot append ED lines to DESC file";

while (<OVERLAP>) {
# Example overlap lines
#Sequence [A0A183JIN8] overlap ShortName EUF00287/349-431 (EUF00287/348-432, 30.7 bits) FULL with Serinc PF03348/10-410 (PF03348/10-416, 374.90 bits) FULL
#Sequence [A0A176VM93] overlap ShortName EUF00287/58-106 (EUF00287/54-106, 32.2 bits) FULL with RVT_1 PF00078/27-86 (PF00078/17-112, 33.70 bits) FULL
#Sequence [A0A151NEE6] overlap ShortName EUF00287/129-170 (EUF00287/125-174, 28.4 bits) FULL with SCAN PF02023/71-137 (PF02023/70-146, 36.80 bits) FULL
#Sequence [W5MUW6] overlap ShortName EUF00287/304-382 (EUF00287/304-401, 43.1 bits) FULL with RNase_H PF00075/367-453 (PF00075/352-454, 46.20 bits) FULL
#Sequence [C5L728] overlap ShortName EUF00287/753-814 (EUF00287/750-849, 25.2 bits) FULL with Peptidase_A17 PF05380/678-816 (PF05380/678-832, 49.30 bits) FULL
#Sequence [A0A0R3SGD1] overlap ShortName EUF00287/47-142 (EUF00287/41-143, 43.7 bits) FULL with DUF155 PF02582/119-208 (PF02582/117-208, 67.10 bits) FULL
#Sequence [A0A074ZPS8] overlap ShortName EUF00287/12-68 (EUF00287/12-71, 36.3 bits) FULL with rve PF00665/30-124 (PF00665/14-126, 60.10 bits) FULL
#Sequence [A0A0B2VY16] overlap RT_RNaseH EUF00288/25-90 (EUF00288/9-99, 24.5 bits) FULL with Peptidase_A17 PF05380/3-105 (PF05380/1-106, 73.30 bits) FULL
#Sequence [A0A151S7W1] overlap RT_RNaseH EUF00288/62-90 (EUF00288/44-99, 23.1 bits) FULL with RVT_1 PF00078/9-78 (PF00078/9-109, 29.00 bits) FULL
#Sequence [A0A164TKU1] overlap RT_RNaseH EUF00288/132-167 (EUF00288/127-177, 27.4 bits) FULL with RVT_1 PF00078/21-139 (PF00078/21-148, 50.50 bits) FULL
#Sequence [A0A151NEE6] overlap RT_RNaseH EUF00288/125-169 (EUF00288/112-172, 30.7 bits) FULL with SCAN PF02023/71-137 (PF02023/70-146, 36.80 bits) FULL
#Sequence [A0A151SI55] overlap RT_RNaseH EUF00288/111-145 (EUF00288/111-149, 25.3 bits) FULL with PAP_fibrillin PF04755/11-189 (PF04755/2-190, 97.90 bits) FULL
#Sequence [C5L728] overlap RT_RNaseH EUF00288/751-809 (EUF00288/744-817, 26.4 bits) FULL with Peptidase_A17 PF05380/678-816 (PF05380/678-832, 49.30 bits) FULL
#Sequence [A0A183JIN8] overlap RT_RNaseH EUF00288/342-418 (EUF00288/333-421, 40.3 bits) FULL with Serinc PF03348/10-410 (PF03348/10-416, 374.90 bits) FULL
#Sequence [A0A183PIT7] overlap RT_RNaseH EUF00288/343-404 (EUF00288/334-408, 26.3 bits) FULL with Serinc PF03348/10-507 (PF03348/10-508, 498.10 bits) FULL
#Sequence [H3HAG7] overlap RT_RNaseH EUF00288/619-651 (EUF00288/619-651, 23.7 bits) FULL with RVT_1 PF00078/527-641 (PF00078/527-643, 29.50 bits) FULL
#Sequence [A0A087GGU6] overlap RT_RNaseH EUF00288/445-505 (EUF00288/444-511, 55.9 bits) FULL with RVT_1 PF00078/329-459 (PF00078/313-463, 76.60 bits) FULL

  if (/Sequence \[(\S+)\] overlap \S+ \S+\/\d+-\d+ \(\S+\/((\d+)-(\d+)), \S+ bits\) \S+ with \S+ \S+\/\d+-\d+ \(\S+\/(\d+)-(\d+), \S+ bits\) \S+/) {
    my $id = $1 . ".1"; # Append .1 version
    my $se = $2;
    my $s = $3;
    my $e = $4;
    my $s2 = $5;
    my $e2 = $6;

    # If overlap is completely enclosed in other one then delete it
    my $ed;
    if ($s>=$s2 and $e<=$e2) {
        $ed="ED   $id/$s-$e;\n";
    } elsif ($s<$s2 and ($s2-$s)>20){
        $ed="ED   $id/$se; $id/$s-".($s2-1).";\n";
    } elsif ($e>$e2 and ($e-$e2+1)>20){
        $ed="ED   $id/$se; $id/".($e2+1)."-$e;\n";
    } else { # Last resort just delete segment
        $ed="ED   $id/$s-$e;\n";
    }

    print DESC $ed;

  } elsif (/Sequence \[(\S+)\] overlap \S+ \S+\/\d+-\d+ \(\S+\/((\d+)-(\d+)), \S+ bits\) \S+ with \S+ \S+\/\d+-\d+ SEED/) {
    my $id = $1 . ".1"; # Append .1 version
    my $se = $2;
    my $s = $3;
    my $e = $4;

    # Overlap to SEED so delete entire region
    my $ed="ED   $id/$s-$e;\n";
    print DESC $ed;

# Also match the overlap resolution overlap format!
#(1) In YflT PF11181 ALIGN : A0A0C2QGG9.1/14-150 (28.3 bits) overlaps with DUF6654 PF20359 ALIGN A0A0C2QGG9.1/13-113 (38.7 bits)
#(2) In DUF6654 PF20359 ALIGN : A0A0K9GGJ3.1/4-76 (30.8 bits) overlaps with YflT PF11181 ALIGN A0A0K9GGJ3.1/6-136 (64.4 bits)
#(2) In YflT PF11181 ALIGN : A0A126PA40.1/10-143 (29.7 bits) overlaps with DUF6654 PF20359 ALIGN A0A126PA40.1/11-150 (35.9 bits)
#3(1) In YflT PF11181 ALIGN : A0A139XGR3.1/14-150 (26.3 bits) overlaps with DUF6654 PF20359 ALIGN A0A139XGR3.1/13-113 (35.3 bits)
#(1) In C2 PF00168 ALIGN : A0A0L7R2L7.1/922-1050 (31.7 bits) overlaps with MUN PF06292 ALIGN A0A0L7R2L7.1/564-970 (48.6 bits)
#(1) In C2 PF00168 ALIGN : A0A0L7R2L7.1/922-1050 (31.7 bits) overlaps with MUN PF06292 ALIGN A0A0L7R2L7.1/564-970 (48.6 bits)

  } elsif (/((\S+\.\d+)\/(\d+)-(\d+)) .* overlaps with \S+ PF\d{5} \S+ \S+\.\d+\/(\d+)-(\d+)/) {
    my $id = $2;
    my $se = $1;
    my $s = $3;
    my $e = $4;
    my $s2 = $5;
    my $e2 = $6;

    # If overlap is completely enclosed in other one then delete it
    my $ed;
    if ($s>=$s2 and $e<=$e2) {
      $ed="ED   $id/$s-$e;\n";
    } elsif ($s<$s2 and ($s2-$s)>20) {
      $ed="ED   $id/$se; $id/$s-".($s2-1).";\n";
    } elsif ($e>$e2 and ($e-$e2+1)>20){
      $ed="ED   $id/$se; $id/".($e2+1)."-$e;\n";
    } else { # Last resort just delete segment
      $ed="ED   $id/$s-$e;\n";
    }

    print DESC $ed;

  } else {
    print STDERR "Unrecognised line in overlap file $_";
  }

}
close OVERLAP;
close DESC;
