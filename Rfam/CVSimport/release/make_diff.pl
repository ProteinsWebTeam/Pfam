#!/usr/local/bin/perl -w

BEGIN {
    $rfam_modules_dir =
	(defined $ENV{'RFAM_MODULES_DIR'})
	    ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
    $bioperl_dir =
        (defined $ENV{'BIOPERL_DIR'})
	    ?$ENV{'BIOPERL_DIR'}:"/pfam/db/bioperl";
}

use lib $bioperl_dir;
use lib $rfam_modules_dir;
use strict;
use Bio::Index::Fasta;
use CRC64;

my $newindex = shift;
my $oldindex = shift;

die "not called correctly" unless $newindex;

my $oldinx = Bio::Index::Fasta->new( '-filename' => $oldindex );
my $newinx = Bio::Index::Fasta->new( '-filename' => $newindex );
my $oldnum = $oldinx -> _file_count() or 0;
my $newnum = $newinx -> _file_count() or 0;

my( %newacc, %oldacc );
for( my $i = 0; $i < $newnum; $i++ ) {
    my( $file ) = $newinx->unpack_record( $newinx->db->{"__FILE_$i"} );
    open( F, $file ) or die;
    while( <F> ) {
	if( /^\>(\S+)/ ) {
	    $newacc{ $1 } = 1;
	}
    }
    close F;
}

for( my $i = 0; $i < $oldnum; $i++ ) {
    my( $file ) = $oldinx->unpack_record( $oldinx->db->{"__FILE_$i"} );
    open( F, $file ) or die;
    while( <F> ) {
    	if( /^\>(\S+)/ ) {
	    $oldacc{ $1 } = 1;
	}
    }
    close F;
}


foreach my $acc ( keys %newacc ) {
    if( not exists $oldacc{ $acc } ) {
	print "$acc NEW\n";
	next;
    }

    my $oldseq = $oldinx -> fetch( $acc );
    my $newseq = $newinx -> fetch( $acc );

    if( not $oldseq ) {
	warn "can't find $acc in your old database\n";
    }
    if( not $newseq ) {
	warn "can't find $acc in your new database\n";
    }


#    if( $newseq -> id() ne $oldseq -> id() ) {
#	print "$acc RENAME ", $oldseq -> id(), " ", $newseq -> id(), "\n";
#    }

    my $oldcrc = CRC64::crc64( $oldseq->seq );
    my $newcrc = CRC64::crc64( $newseq->seq );

    if( $oldcrc ne $newcrc ) {
	print "$acc SEQCHANGE\n";
    }
}

foreach my $acc ( keys %oldacc ) {
    if( not exists $newacc{ $acc } ) {
	print "$acc DELETED\n";
    }
}


##################

sub next_acc {
    my $fh = shift;
    my $acc;
    while(<$fh>) {
	last if /^\/\//;
	if( /^AC\s+(\S+);/ ) {
	    $acc = $1 unless $acc;
	}
    }
    return $acc;
}
