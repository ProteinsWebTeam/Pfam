#!/usr/local/bin/perl
use lib '/nfs/WWWdev/SANGER_docs/cgi-bin/Rfam';

use RfamWWWConfig;
use EntryWWW;

use lib '/nfs/team71/pfam/mm1/rfam/scripts/Modules';

require "/nfs/team71/pfam/mm1/rfam/scripts/Modules/UpdateRDB.pm";
require "/nfs/team71/pfam/mm1/rfam/scripts/Modules/Bio/Rfam.pm";

use strict;

my $database = shift;
die "Need database to upload dopey!!! \n" if(!$database);

my $rdb;

if ($database =~ /rfam2/) {
 # print "rfam2 \n";
  $rdb = Bio::Rfam->switchover_rdb_update();
} else {
  print "rfam \n";
  $rdb = Bio::Rfam->rdb_update();
}

#exit(0);
print "RDB: $rdb \n";


$rdb->add_rfamseq(  "/nfs/team71/pfam/mm1/RESTORES/Rfam/embl.dat");

print "DONE \n";
