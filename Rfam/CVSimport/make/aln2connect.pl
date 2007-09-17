#!/software/bin/perl -w

#use lib '/pfam/db/bioperl';
#use lib "$ENV{HOME}/rfam/scripts/Modules";
use Rfam::RfamAlign;

my $file = shift;
my $id   = shift;
my $aln = Rfam::RfamAlign->new();
open( F, $file ) or die;
$aln -> read_stockholm( \*F );
$aln -> write_connect( \*STDOUT, $id );
