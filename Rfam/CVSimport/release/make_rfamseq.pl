#!/usr/local/bin/perl -w

# first download all required .dat files to directory
# then run this!


##########
# before next release change this to include missing genome entries
##########

use strict;
use Getopt::Long;
use Bio::SeqIO;
use Bio::Index::Fasta;
use Bio::Index::EMBL;


my( $verbose, @filelist );
&GetOptions( "v"    => \$verbose,
	     "f=s@" => \@filelist );

my $dir = shift;
$dir = `pwd` if not $dir;
chomp $dir;

if( $dir !~ /\// ) {
    die "Must give absolute path to directory [$dir]!";
}

chdir "$dir" or die "can't cd to $dir";
@filelist = glob( "*.dat" ) unless( @filelist );

# convert embl format to fasta
# we want to be keyed off accession rather than id so change disply_id

my @sv;
foreach my $file ( @filelist ) {
    my( $root ) = $file =~ /^(\S+)\.dat/;
    my $in  = Bio::SeqIO->new( -file => $file, '-format' => 'EMBL' );
    my $out = Bio::SeqIO->new( -file => ">$root.fa", '-format' => 'Fasta' );
    while ( my $seq = $in->next_seq() ) {
	print STDERR $seq->accession, "\n" if $verbose;
	$seq -> display_id( $seq->accession.".".$seq->seq_version );
	$out -> write_seq( $seq );
	push( @sv,  $seq->accession.".".$seq->seq_version );
    }
}

# build blast indices
# do this in a new loop to avoid race condition

my( @falist, @datlist );
foreach my $file ( @filelist ) {
    my( $root ) = $file =~ /^(\S+)\.dat/;
    system "formatdb -i $root.fa -p F" and die;
    system "xdformat -I -n $root.fa" and die;
}

# make fasta bioperl index

system "/pfam/db/Rfam/scripts/release/index_rfamseq.pl rfamseq.fa.bpi";

# make embl_sv.txt
open( F, ">embl_sv.txt" ) or die;
foreach my $sv ( sort @sv ) {
    print F "$sv\n";
}
close F;

