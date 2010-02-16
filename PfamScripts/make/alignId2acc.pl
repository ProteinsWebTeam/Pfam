#!/usr/local/bin/perl
#
# Takes a file in "pfam" format, reads it in and looks up the sequence accessions
# based on the ID. Currently only works on UniProt sequences.  
#
# alignId2acc.pl
#
# Author:        rdf
# Maintainer:    $Id$
# Version:       $Revision$
# Created:       Feb 16, 2010
# Last Modified: $Date$


use strict;
use warnings;
use File::Copy;

use Bio::Pfam::Config;
use Bio::Pfam::PfamDBManager;
use Bio::Pfam::AlignPfam;


my $file = shift;

unless(defined($file) and -e $file){
  warn "You either did not specify a file name or it does not exist\n"; 
  usage();
}


copy($file, "$file.ids") 
  or die "Could not copy $file to $file.id:[$!]\n";

open(FH, "$file.ids") or die "Could not open up alignment\n";

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamDBManager->new( %{ $config->pfamlive } );

my $aliWithIds  = Bio::Pfam::AlignPfam->new;
$aliWithIds->read_Pfam(\*FH);
my $aliWithAccs = Bio::Pfam::AlignPfam->new;

foreach my $s ( $aliWithIds->each_seq ){
  if($s->id !~ /\_/){
    warn $s->id ." does not look like an UniProt identifier, skipping\n";
    next;
  }
  my $sObj = $pfamDB->getPfamseqById($s->id);
  $s->id($sObj->pfamseq_acc);
  $s->version($sObj->seq_version);
  $aliWithAccs->add_seq($s);
}


open(my $fh, ">$file" ) or die "Could not open up $file for writing:[$!]\n";
$aliWithAccs->write_Pfam($fh);
close($fh);

#-------------------------------------------------------------------------------

sub usage {
  print STDERR "\n$0 <filename>\n\n"; 
  exit;
}

=head1 alignId2acc

 Takes a file in "pfam" format, reads it in and looks up the sequence accessions
 based on the ID. Currently only works on UniProt sequences.  


=head1 DESCRIPTION

  This script take a sequence alignment defined by the user and tries to 
  find accessions for each of the sequences that are assumed to be in
  the format of UniPort IDs.  Any IDs not found will be ommited from the
  resulting alignment. This is really intended for taking a release Pfam-A or
  Pfam-B file and getting in to a shape the we need it for internal production.
  
  Points to remember
  * This over writes the existing file, but saves the old file as "filename".ids.
  * Assumes Pfam format
  * Assumes that name is UniProt ID in name/start-end
   
$Id$

=head1 COPYRIGHT

File: alignId2acc.pl

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
