#!/usr/local/bin/perl
use lib '/nfs/WWWdev/SANGER_docs/cgi-bin/Rfam';

use RfamWWWConfig;
use EntryWWW;

use lib '/nfs/team71/pfam/mm1/rfam/scripts/Modules';

require "/nfs/team71/pfam/mm1/rfam/scripts/Modules/UpdateRDB.pm";
require "/nfs/team71/pfam/mm1/rfam/scripts/Modules/Bio/Rfam.pm";

use strict;

my  $rdb = Bio::Rfam->rdb_update();
print "RDB: $rdb \n";


$rdb->add_rfamseq(  "/nfs/team71/pfam/mm1/scripts/Rfam/embl.dat");

print "DONE \n";
