#!/usr/local/bin/perl -w

use strict;
use Getopt::Long;
use IO::File;
use Rfam;
use Rfam::RfamAlign;
use CMResults;
use Bio::Tools::BPlite;
my $agp      = shift;
my $rdb = Rfam::switchover_rdb();
my %hits;
my( $rfamacc, $rfamid, $ready );
my %annseq;
my %agp;
my $chr;
open( AGP, $agp ) or die;
while(<AGP>) {

    if( my( $chr, $chrst, $chren, $clone, $clst, $clen, $clstr ) = 
	/^(\S+)\s+(\d+)\s+(\d+)\s+\S+\s+\S+\s+(\S+)\s+(\d+)\s+(\d+)\s+([+|-])/ ) {
print_hits( $clone, $clst, $clen, $chr, $chrst, $chren, $clstr);
    }
}
close AGP;
sub print_hits {
    my( $sv, $constart, $conend, $chromosome, $chrom_start, $chrom_end, $orient ) = @_;
    my( $seqid ) = $sv =~ /^(\S+)\.\d+/;
    my @annseqs;
    if( $annseq{$sv} ) {
	push( @annseqs, $annseq{$sv} );
    }
    else {
      GET: {
	  eval {
	      @annseqs = $rdb -> get_AnnotSeqs( [$sv], ['full'] );
	  };
	  if( $@ ) {
	      print STDERR "fail\n";
	      sleep 2;
	      redo GET;
	  }
      }
    }
    foreach my $annseq ( @annseqs ) {
	foreach my $reg ( $annseq->eachAnnotatedRegion ) {
	    if( $reg->from > $constart and 
		$reg->from < $conend   and
		$reg->to   > $constart and
		$reg->to   < $conend ) {
		my( $start, $end);
                if( $orient eq "+" ) {
                    $start = ($chrom_start - $constart) + $reg->from;
                    $end   = ($chrom_start - $constart) + $reg->to;
                }
                else {
                    $start = $chrom_start + $conend - $reg->from;
                    $end   = $chrom_start + $conend - $reg->to;
                }
		
		#printf( "RF   %-26s %-28s %-5s %-10s %-16s %7s\n", $reg->rfamseq_id()."/".$reg->from."-".$reg->to, $chromosome.":".$start."-".$end, $orient, $reg->accession, $reg->id, sprintf( "%.2f", $reg->bits_score ) );
	        printf( "%-10s %-12s %-12s %-5s %-15s %-12s %-12s %-10s %-16s %7s\n",$chromosome, $start, $end, $orient, $reg->rfamseq_id(), $reg->from, $reg->to, $reg->accession, $reg->id, sprintf( "%.2f", $reg->bits_score ) );
	    }
	}
    }
}
