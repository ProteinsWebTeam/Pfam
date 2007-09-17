#!/software/bin/perl -w

use strict;
use Getopt::Long;
use Rfam;

my( $a, $b );
GetOptions( "a=s" => \$a,
	    "b=s" => \$b );

my $rfamseq = $Rfam::rfamseq;
my $name = shift;

if( $name =~ /(\S+)\/(\d+)-(\d+)/ ) {
    $name = $1;
    $a = $2;
    $b = $3;
}

my $options = "-n";
my( $st, $en ) = ( $a, $b );

if( $a and $b and $a > $b ) {
    $options .= " -r";
    ( $st, $en ) = ( $b, $a );
}

$options .= " -a $st" if( $a );
$options .= " -b $en" if( $b );

open( XD, "xdget $options $rfamseq $name |" ) or die;
while(<XD>) {
    if( /^\>(\S+)\s+(.*)\s+\{.*\}/ ) {
	print ">$1/$a-$b  $2\n";
	next;
    }
    print;
}
