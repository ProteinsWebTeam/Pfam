#!/usr/bin/perl -T
#
# Author:        rdf
# Maintainer:    $Author$
# Version:       $Revision$
# Created:       May 13, 2010
# Last Modified: $Date$

#This needs to be configured in apache ideally, but it works and so I am going with this......
use lib '/nfs/production/xfam/pfam/software/Pfam/PfamSchemata';
use lib '/nfs/production/xfam/pfam/software/Pfam/PfamLib';
use lib '/nfs/production/xfam/pfam/rh7/perl5/lib/perl5';
use lib '/usr/share/perl5/'; #For CGI.pm


use strict;
use warnings;

use CGI;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my $q = CGI->new;
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

unless($pfamDB){
    bail("Could not connect to pfam database");  
}

#Need to get a list of all pfam ids;
my @dufNos;
my $pfams    = $pfamDB->getAllPfamFamilyDataLike('DUF%');
my $deadFams = $pfamDB->getAllDeadFamilyData; 
foreach my $fam (@{$pfams}, @{$deadFams}){
  if($fam->pfama_id =~ /^DUF(\d+)$/){
    push(@dufNos, $1); 
  }
}

#Sort them so we can work out the next number.
my @dufNosSort = sort {$b <=> $a} @dufNos;
my $no = $dufNosSort[0] + 1;

#Now return the overlaps
print $q->header(), "The next available DUF number is: DUF$no\n";

exit;

sub bail {
  my $msg = shift;
  print $q->header( -status => '400 Bad Request' ), $msg;
  exit 1;
}
