#!/usr/local/bin/perl

#-------------------------------------------------------------------------------

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Alex Bateman, <agb at sanger.ac.uk> K Cara Woodwark, <cara at sanger.ac.uk>
Maintainer: Rob Finn, <rdf at sanger.ac.uk>

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.


=cut

#-------------------------------------------------------------------------------

use strict;
use warnings;
use Getopt::Long;
use Cwd;

use Bio::Pfam::Config;
use Bio::Pfam::PfamDBManager;

unless ( scalar(@ARGV) ) { useage(); }

my $config = Bio::Pfam::Config->new;
$config->dieIfNotWTSI;


my $pfamDB = Bio::Pfam::PfamDBManager->new( %{ $config->pfamlive } );


my $acc;
my $id;
my $help;
my $dir  = getcwd;
my $file;
my $score;

GetOptions(
  'h|help' => \$help,
  'acc=s'  => \$acc,
  'id=s'   => \$id,
  'dir=s'  => \$dir,
  'file=s' => \$file,
  'score=s' => \$score
) or print STDERR "Invalid option passed in" and usage();


useage() if $help;
#Need an accession or id to work
useage() unless $acc || $id;

my $entry;
if($acc){
  $entry = $acc;
}else{
  $entry = $id;
}

#Check that the entry is valid?
my $famData = $pfamDB->getPfamData($entry);
unless($famData and $famData->pfama_id){
  die "$entry is was not found in the MySQL database. ".
      "Has this family been delete or miss typed?\n";
}

#Where are we going to write to!
my $fh;
if ($file) {
  $file = $dir . "/" . $file;
  open( $fh, ">$file" ) or die "Problem opening $file: $!\n";
}
else {
  $fh = \*STDOUT;
}

#Set the score
$score = (defined($score) ? $score : "10.0" );
#Now perform the query!
my $results = $pfamDB->getScoopData($famData, $score);

#Print the results;
print $fh sprintf("%-10s ", '#score').
          sprintf("%-9s " , 'PfamA_Id').
          sprintf("%-16s " , 'PfamA_acc').
          sprintf("%-9s " , 'PfamA_Id').
          sprintf("%-16s\n" , 'PfamA_acc');
print $fh "#".("-" x 60)."\n";          

if($results and ref($results) eq 'ARRAY' and scalar(@$results)){
foreach my $r (@$results){
  print $fh sprintf("%-10.1f", $r->get_column('score'))." ";  
  if($r->get_column('l_pfama_id') eq $famData->pfama_id){
    print $fh sprintf("%-9s " , $r->get_column('r_pfama_acc')).
              sprintf("%-16s ", $r->get_column('r_pfama_id')).
              sprintf("%-9s " , $r->get_column('l_pfama_acc')).
              sprintf("%-16s\n", $r->get_column('l_pfama_id'));  
  }else{
    print $fh sprintf("%-9s " , $r->get_column('l_pfama_acc')).
              sprintf("%-16s ", $r->get_column('l_pfama_id')).
              sprintf("%-9s " , $r->get_column('r_pfama_acc')).
              sprintf("%-16s\n", $r->get_column('r_pfama_id'));
  } 
  
}
}else{
  print STDERR "No results found\n";
}
close($fh);
  
sub useage {
  exec( 'perldoc', $0 );
}

=head1 USEAGE



useage:
	get_scoop.pl -acc PF01234 -dir /tmp -file PF01234_scoop.out 
	
	May be run with either -acc or -id 
	
	Results are redirected to STDOUT but may be sent to the file given by the -file flag (as shown above).
	
	defaults:

		dir	current_working_dir

=head1 DESCRIPTION

Pulls scoop data from MySQL database for family of interest		
		
=head1 AUTHOR

=cut

1;
exit;

