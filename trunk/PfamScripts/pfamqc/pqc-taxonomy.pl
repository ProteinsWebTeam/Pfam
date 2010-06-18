#!/usr/local/bin/perl

# pqc-taxonomy.pl
#
# Author:        rdf
# Maintainer:    $Author$
# Version:       $Revision$
# Created:       Apr 8, 2010
# Last Modified: $Date$

=head1 NAME

 pqc-taxonomy.pl - gives the taxonomic distribution of the family

=cut

=head1 DESCRIPTION

This goes through the alignment, uploads the sequences to the databases in a 
temporary table, and determines the lowest common taxonomic lineage.  It retrieves
what was the previously lowest common taxonomic lineage and will warn if the
value has changes. This could be due to the fact that the alignment has expanded
or redeued it's taxonomic distribution.

$Author$

=head1 COPYRIGHT

File: pqc-taxonomy.pl

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

use strict;
use warnings;
use DBI; #This is going to have to go outside of DBIx::Class
use Cwd;

use Bio::Pfam::Config;
use Bio::Pfam::FamilyIO;

my $config = Bio::Pfam::Config->new;
$config->dieIfNotWTSI;

my $family = shift;


#Now load the local alignment
#-------------------------------------------------------------------------------
#Get local and SVN based copies of the family.
my $pwd          = getcwd();
my $familyIO     = Bio::Pfam::FamilyIO->new;
my $localFamObj  = $familyIO->loadPfamAFromLocalFile( $family, $pwd );




my $dbh = DBI->connect( "dbi:mysql:database=".$config->pfamlive->{database}.
                                 ";host=".$config->pfamlive->{host}.
                                 ";port=".$config->pfamlive->{port},
                                 $config->pfamlive->{adminuser},
                                 $config->pfamlive->{adminpassword}) or die $DBI::errstr;;

#-------------------------------------------------------------------------------
#Set up a temporary table
my $table = "_taxDist$$"; #Keep the table name for later use.        
my $statement = "create temporary table $table ( `pfamseq_acc` varchar(6) NOT NULL,".
                "PRIMARY KEY (pfamseq_acc) )";
$dbh->do($statement) or die $dbh->errstr;


#now populate that table
my ( $seqAccs, %seen );

foreach my $seq ($localFamObj->ALIGN->each_seq ){
  next if($seen{$seq->id}); 
  $seqAccs .= "\'".$seq->id."\'),(";
  $seen{$seq->id}++;
}

#Remove the last ),( from the string
$seqAccs = substr( $seqAccs, 0, length($seqAccs) - 3);

my $insertStm = "insert into $table values ( $seqAccs )";
$dbh->do($insertStm) or die $dbh->errstr;

#-------------------------------------------------------------------------------
#Now grab the different taxonomic ranges;
my $ranges  = $dbh->selectall_arrayref('select lft, rgt, level from taxonomy where parent="root"')
                                          or die $dbh->errstr;
                                         
                                         

#Now loop over the tax ranges and perform a look-up on each if there is an overlap
my $sthTaxDepth = $dbh->prepare( "SELECT level, lft, rgt from taxonomy 
                                  WHERE lft<= ? and rgt>= ?
                                  AND level!='NULL' 
                                  ORDER BY rgt-lft DESC") or die $dbh->errstr();

my $sthMinMax   = $dbh->prepare("SELECT min(lft), max(rgt), count(auto_pfamseq) 
                                            FROM  $table r, pfamseq s, taxonomy t 
                                            WHERE s.pfamseq_acc=r.pfamseq_acc 
                                            AND t.ncbi_taxid=s.ncbi_taxid 
                                            AND t.ncbi_taxid!=0 
                                            AND lft >= ?
                                            AND rgt <= ?" ) or die $dbh->errstr();
my (%depth, %count);
foreach my $r (@$ranges){
  #Test to see if it falls in range
  #Now get the max/min range for the family based on the temp table.
  $sthMinMax->execute($r->[0], $r->[1]);
  my ($min, $max, $count) = $sthMinMax->fetchrow_array;
                                          
  next unless($min and $max);              
  $sthTaxDepth->execute($min, $max);
  foreach my $tax ( @{ $sthTaxDepth->fetchall_arrayref } ){
    $depth{$r->[2]} .= $tax->[0].";";    
  }
  $count{$r->[2]} = $count;
}
$sthTaxDepth->finish;
$sthMinMax->finish;



#Disconnct that will destroy the temporary table.
$dbh->disconnect;

#-------------------------------------------------------------------------------
#Print out the results in some table
print STDERR sprintf("%-7s %-22s %-6s %s\n", "Family", "Kingdom","Count", "Lowest Common");
print STDERR "=" x 80; 
print STDERR "\n";

foreach my $king (keys %depth){
  print STDERR sprintf( "%-7s %-22s %-6d %s\n", $family, $king, $count{$king}, $depth{$king} );  
}


#-------------------------------------------------------------------------------

