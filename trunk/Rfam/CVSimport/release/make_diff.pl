#!/usr/local/bin/perl -w

use strict;
use POSIX;   # gives us floor() function
use Rfam;
use Bio::SeqIO;
use Bio::Index::Fasta;

my $newlist = shift;
my $oldlist = shift;

die "not called correctly" unless $oldlist;
my $inx = Bio::Index::Fasta -> new( '-filename' => $Rfam::rfamseq_current_inx,
				    '-dbm_package' => 'DB_File' );

my( %newacc, %oldacc );
open( F, $newlist ) or die;
while( <F> ) {
    if( /^(\S+)\.(\d+)/ ) {
	$newacc{ $1 } = $2;
    }
}
close F;

open( F, $oldlist ) or die;
while( <F> ) {
    if( /^(\S+)\.(\d+)/ ) {
	$oldacc{ $1 } = $2;
    }
}
close F;

my $i = 0;
my $k = 1;
my $out = Bio::SeqIO->new( '-file' => ">new.fa.1", '-format' => 'Fasta' );

foreach my $acc ( keys %newacc ) {
    my $seq;
    if( not exists $oldacc{ $acc } ) {
	print "$acc.$newacc{$acc} NEW\n";
	$seq = $inx -> fetch( $acc.".".$newacc{$acc} );
    }
    elsif( $newacc{ $acc } != $oldacc{ $acc } ) {
	print "$acc.$newacc{$acc} UPDATE\n";
	$seq = $inx -> fetch( $acc.".".$newacc{$acc} );
    }
    
    if( $i >= 5000 ) {
	$k ++;
	$i = 0;
	$out = Bio::SeqIO->new( '-file' => ">new.fa.$k", '-format' => 'Fasta' );
    }
    if( $seq ) {
	$out -> write_seq( $seq );
	$i ++;
    }
}

foreach my $acc ( keys %oldacc ) {
    if( not exists $newacc{ $acc } ) {
	print "$acc.$oldacc{$acc} DELETED\n";
    }
}

