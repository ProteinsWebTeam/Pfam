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
use Getopt::Long;
use Bio::SeqIO;

my( $verbose, @filelist );
&GetOptions( "v"    => \$verbose,
	     "f=s@" => \@filelist );

my $dir = shift;
$dir = "." if not $dir;
chdir "$dir" or die "can't cd to $dir";
@filelist = glob( "*.dat" ) unless( @filelist );

# convert embl format to fasta
# we want to be keyed off accession rather than id so change disply_id

foreach my $file ( @filelist ) {
    my( $root ) = $file =~ /^(\S+)\.dat/;
    my $in  = Bio::SeqIO->new( -file => $file, '-format' => 'EMBL' );
    my $out = Bio::SeqIO->new( -file => ">$root.fa", '-format' => 'Fasta' );
    while ( my $seq = $in->next_seq() ) {
	print STDERR $seq->accession, "\n" if $verbose;
	$seq -> display_id( $seq->accession.".".$seq->seq_version );
	$out -> write_seq( $seq );
    }
}

# build blast indices
# do this in a new loop to avoid race condition

foreach my $file ( @filelist ) {
    my( $root ) = $file =~ /^(\S+)\.dat/;
    system "formatdb -i $root.fa -p F" and die;
}

# make fasta and embl bioperl indices
# not implemented yet

