#!/usr/local/bin/perl -w

# first download all required .dat files to directory
# then run this!

BEGIN {
    $bioperl_dir =
        (defined $ENV{'BIOPERL_DIR'})
	?$ENV{'BIOPERL_DIR'}:"/pfam/db/bioperl";
}

use lib $bioperl_dir;

use strict;
use Bio::SeqIO;

my $dir = shift;
$dir = "." if not $dir;
chdir "$dir" or die "can't cd to $dir";
my @filelist = glob( "*.dat" );

foreach my $file ( @filelist ) {
    my( $root ) = $file =~ /^(\S+)\.dat/;
    my $in  = Bio::SeqIO->new( -file => $file, '-format' => 'EMBL' );
    my $out = Bio::SeqIO->new( -file => ">$root.fa", '-format' => 'Fasta' );
    while ( my $seq = $in->next_seq() ) {
	$seq -> display_id( $seq -> accession );
	$out -> write_seq( $seq );
    }
}
