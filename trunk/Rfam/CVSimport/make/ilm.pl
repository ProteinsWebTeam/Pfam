#!/usr/local/bin/perl -w

# alignment in, alignment with structure out

use strict;
use Getopt::Long;
use IO::File;
use Rfam;
use Rfam::RfamAlign;

my( $help, $hlxwt, $mixywt );

GetOptions( "h"     => \$help,
	    "hwt=s" => \$hlxwt,
	    "mwt=s" => \$mixywt,
	    );

sub usage {
    print STDERR <<EOF;
Usage: $0 <alignfile>

EOF
}

my $file = shift;
if( not $file or $help ) {
    &usage();
    exit(1);
}

open( ALN, $file ) or die;
my $aln = Rfam::RfamAlign->new();
$aln -> read_stockholm( \*ALN );
close ALN;

open( TA, ">/tmp/$$.aln" ) or die;
$aln -> write_ilm( \*TA );
close TA;

my $ss = Rfam::SS -> new();
system( "mixy /tmp/$$.aln > /tmp/$$.mixy" ) and die;
system( "hlxplot /tmp/$$.aln > /tmp/$$.hlx" ) and die;

$hlxwt = 20/scalar($aln->each_seq) unless( defined $hlxwt );
$mixywt = 1000                     unless( defined $mixywt );

open( DAT, ">/tmp/$$.dat" ) or die;
print DAT "/tmp/$$.mixy $mixywt\n";
print DAT "/tmp/$$.hlx $hlxwt\n";
close DAT;

system( "makegraf /tmp/$$.dat > /tmp/$$.mtx" ) and die;
system( "ilm /tmp/$$.mtx > /tmp/$$.col 2>/dev/null" ) and die;

open( COL, "/tmp/$$.col" ) or die;
while(<COL>) {
    last if( /^Final Matching/ );
}
while(<COL>) {
    my( $left, $right ) = split;
    if( $left and $right ) {
	next if $ss->pairedBase( $left );
	my $pair = Rfam::Pair -> new( $left, $right );
	$ss->addPair($pair);
#	print "$left $right\n";
    }
}
close COL;

$ss -> get_knotted();

$aln->ss_cons( $ss );
$aln->write_stockholm( \*STDOUT );

unlink "/tmp/$$.aln" or die;
unlink "/tmp/$$.mixy" or die;
unlink "/tmp/$$.hlx" or die;
unlink "/tmp/$$.dat" or die;
unlink "/tmp/$$.mtx" or die;
unlink "/tmp/$$.col" or die;
