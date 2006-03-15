use lib 't';
use strict;
use warnings;

use Test;
use TestUtils;

use Bio::Pfam::DB_RCS;

$| = 1;

plan tests => 3;

# 1 - compiles
ok(1);

# 2 - get DB_RCS object

my $pfamroot = '/pfam/db/Pfam';
my $db = Bio::Pfam::DB_RCS -> new( '-current'      => "$pfamroot/CURRENT",
                                   '-attic'        => "$pfamroot/RCS_ATTIC",
                                   '-index'        => "$pfamroot/ACCESSION/accmap.dat", 
                                   '-lock_file'    => "$pfamroot/ACCESSION/lock", 
                                   '-fastadb'      => "/pfam/db/pfamseq/pfamseq", 
                                   '-srsdb'        => "pfamseq",
                                   '-srspfamb'     => "pfambnew", 
                                   '-srsswisspfam' => "swisspfamnew", 
                                  );
ok( $db->isa('Bio::Pfam::DB_RCS') );

# 3 - find an id from an accession
ok( $db->acc2id('PF00001'), "7tm_1" );

# 4 - get a Pfam entry
my $entry = $db->get_EntryA_by_acc('PF00001');
ok( $entry->isa('Bio::Pfam::EntryA_RCS') );

