#!/usr/local/bin/perl -w

use strict;
use lib '/nfs/disk100/pubseq/Pfam/bioperl';
use Bio::SimpleAlign;
use lib '/pfam/db/Rfam/scripts/Modules';
use Rfam;

my $acc = shift;
chdir "$current_dir/$acc" or die;

my @ann;
open( DESC, "DESC" ) or die;
while( <DESC> ) {
    unless( /^\*\*\s+/ ) {
	push( @ann, "#=GF $_" );
    }
}
close DESC;

foreach my $file ( @align_file_set ) {
    my $aln = new Bio::SimpleAlign;
    open( ALN, $file ) or die;
    $aln -> read_selex( \*ALN );
    close ALN;
    my $numseq = scalar ( $aln -> eachSeq() );

    my %accessions;
    open( TEMP, ">temp.$$" ) or die;
    foreach my $seq ( $aln -> eachSeq() ) {
	my $id = $seq->id();
	print TEMP "emblrelease:$id\n";
    }
    close ALN;
    close TEMP;

    my $id;
    open( GETZ, "getz -f id -f acc \@temp.$$ |" ) or die;
    while(<GETZ>) {
	if( /^ID\s+(\S+)\s+/ ) {
	    $id = $1;
	}
	if( /^AC\s+(\S+)\s*\;\s*/ ) {
	    $accessions{$id} = $1;
	}
    }
    close GETZ or die;
    unlink "temp.$$" or die "can't remove temp.$$";

    open( ALNOUT, ">$file.ann" ) or die;

    my $seen;
    open( REF, "sreformat --mingap stockholm $file |" ) or die;
    while( <REF> ) {
	next if( /^\#=GF AU    CM RNA automatic alignment/ );
	if( /^\#=G/ and not $seen ) {
	    print ALNOUT @ann;
	    print ALNOUT "#=GF SQ   $numseq\n";
	    $seen = 1;
	}
	elsif( /^\S+\/\d+-\d+/ and not $seen ) {
	    print ALNOUT @ann;
	    print ALNOUT "#=GF SQ   $numseq\n\n";
	    $seen = 1;
	}
	if( /^((?:\#=GR )?(\S+)\/\d+-\d+\s+)(.*)\s*$/ ) {
	    my( $nse, $id, $seq ) = ( $1, $2, $3 );
	    my $length = length $nse;
	    die "$id not found in your sequence database" if( not $id );
	    $nse =~ s/$id/$accessions{$id}/g;
	    $nse =~ s/\s+$//;
	    my $format = "\%-".$length."s%s\n";
	    printf ALNOUT ( "$format", $nse, $seq );
	}
	elsif( /^(\#=GS (\S+)\/\d+-\d+)\s+(.*)\s*$/ ) {
	    my( $nse, $id, $rest ) = ( $1, $2, $3 );
	    die "$id not found in your sequence database" if( not $id );
	    $nse =~ s/$id/$accessions{$id}/g;
	    printf ALNOUT ( "%-34s%s\n", $nse, $rest );
	}
	else {
	    print ALNOUT;
	}
    }
    close REF or die;
    close ALNOUT;
}

