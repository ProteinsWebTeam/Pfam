#!/usr/bin/env perl
#
#
# Author:        jm14
# Maintainer:    $Id$
# Version:       $Revision$
# Created:       Jun 22, 2009
# Last Modified: $Date$

=head1 NAME

merge_alignment.pl - merges two alignments
 
=cut


=head1 DESCRIPTION

Script that takes two input alignments, reformat them and makes a single alignment
using the specified alignment method.Template - a short description of the class

$Id$

=head1 COPYRIGHT

File: merge_alignment.pl

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

 This is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 or see the on-line version at http://www.gnu.org/copyleft/gpl.txt
 
=cut

#General modules from CPAN
use strict;
use warnings;
use Getopt::Long;

#Our modules
use Bio::Pfam::AlignMethods;
use Bio::Pfam::Config;


#Can you not use $0? (rdf)
my $prog = "merge_alignment.pl";


#-------------------------------------------------------------------------------
# Deal with the user input

my (
  $fasta1, $fasta2,  $t_coffee, $mafft,$muscle, $musclep, $method,$mcount
);

&GetOptions(
  'fasta=s'  => \$fasta1,
  'fasta2=s' => \$fasta2,
  'm!'       => \$mafft,
  'mu!'      => \$muscle,
  'mup!'     => \$musclep,
);

if ($mafft) {
  $method = "mafft";
  $mcount++;
}
if ($muscle) {
  $method = "muscle";
  $mcount++;
}
if ($musclep) {
  $method = "musclep";
  $mcount++;
}
if ( $mcount == 0 ) {
  &Bio::Pfam::AlignMethods::help($prog);
}

if ( $mcount != 1 ) {
  die "More than one alignment method selected!\n";
}

if ( !$fasta1 || !$fasta2 ) {
  &Bio::Pfam::AlignMethods::help($prog);
}
if ( !-s $fasta1 || !-s $fasta2 ) {
  die "Your fasta file either does not exist or is of zero size\n";
}

#-------------------------------------------------------------------------------
# Join the two input files.
my $fasta_file = "totalfa.$$";
foreach my $file ($fasta1, $fasta2) {
  my $tmp_file = "tmp.$$";
  my $grep_command = "grep -v ^// $file > $tmp_file";  #Remove // if present
  system("$grep_command") and die "Couldn't run $grep_command, $!"; 
  system("esl-reformat -u fasta $tmp_file >> $fasta_file") and die "Couldn't run esl-reformat -u fasta $tmp_file >> $fasta_file, $!";
  unlink($tmp_file);
}


my $config = Bio::Pfam::Config->new;

# Read fasta file and put ref into scalars
my ($sequence, $description)  = &Bio::Pfam::AlignMethods::read_fasta($fasta_file);


# Create alignment
my %hash =  &Bio::Pfam::AlignMethods::create_alignment($sequence, $description, $method, $fasta_file);

#Print alignment
&Bio::Pfam::AlignMethods::print_alignment( \%hash, $method );

unlink($fasta_file);
exit(0);
