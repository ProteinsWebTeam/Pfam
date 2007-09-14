#!/software/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/software/rfam/scripts/Modules";
    $bioperl_dir =
        (defined $ENV{'BIOPERL_DIR'})
            ?$ENV{'BIOPERL_DIR'}:"/lustre/pfam/db/bioperl";
}

use lib $bioperl_dir;
use lib $rfam_mod_dir;

use Rfam::RfamAlign;

my $file = shift;
open( F, $file ) or die;
my $aln = new Rfam::RfamAlign;
$aln -> read_stockholm( \*F );
$aln -> write_structure_ps( \*STDOUT );
