#!/usr/local/bin/perl -w

use strict;
use Getopt::Long;
use Rfam::RfamAlign;

my $fontsize = 10;
GetOptions( "f=s" => \$fontsize );

open( F, shift ) or die;
my $aln = Rfam::RfamAlign->new();
$aln->read_stockholm( \*F );
$aln->write_coloured_ps( \*STDOUT, $fontsize );
