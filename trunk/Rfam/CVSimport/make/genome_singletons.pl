#!/usr/local/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
    $bioperl_dir =
        (defined $ENV{'BIOPERL_DIR'})
            ?$ENV{'BIOPERL_DIR'}:"/pfam/db/bioperl";
}

use lib $rfam_mod_dir;
use lib $bioperl_dir;
use strict;
use Getopt::Long;

use Bio::Tools::BPlite;

my( $agp, $con, $fasta );
&GetOptions( "agp"     => \$agp,
	     "con"     => \$con,
	     "fasta"   => \$fasta );

my $sp2dir  = "/pfam/db/SP2/CURRENT";
my @fafiles = glob( "$sp2dir/*.fa" );

my $blastprog = "/usr/local/pubseq/bin/wu_blast/wublastn";

my $file = shift;
my @regions;
unless( $fasta ) {
    open( E, $file ) or die;
    if( $agp ) {
	while(<E>) {
	    if( /^\S+\s+\d+\s+\d+\s+\d+\s+\S+\s+(\S+)\.(\d+)\s+(\d+)\s+(\d+)/ ) {
		my( $id, $ver, $st, $en ) = ( $1, $2, $3, $4 );
		push( @regions, { 'id' => $id, 'ver' => $ver, 'start' => $st, 'end' => $en } );
	    }
	}
    }
    elsif( $con ) {
	while(<E>) {
	    while( /([A-Z0-9]+)\.(\d+)\:(\d+)\.\.(\d+)/g ) {
		my( $id, $ver, $st, $en ) = ( $1, $2, $3, $4 );
		push( @regions, { 'id' => $id, 'ver' => $ver, 'start' => $st, 'end' => $en } );
	    }
	}
    }
    close E;

    foreach ( @regions ) {
	my( $id, $ver, $st, $en ) = ( $_->{'id'}, $_->{'ver'}, $_->{'start'}, $_->{'end'} );
	system "pfetch $id\.$ver:$st-$en -n $id\.$ver\/$st-$en >> $$.fa" and die;
    }
}

if( $fasta ) {
    $fasta = $file;
}
else {
    $fasta = "$$.fa";
}

system "pressdb $fasta > /dev/null" and die "can't formatdb $fasta\n";

my %hits;
foreach my $file ( @fafiles ) {
    system "$blastprog $fasta $file -E0.01 -W3 >> $$.blast" and die;

    open( B, "$$.blast" ) or die;
    my $report = new Bio::Tools::BPlite( -fh => \*B );
    {
	my( $qname, $qlength ) = $report->query =~ /^(.*)\s+\((\d+)\s+letters/;
	next unless $qlength;           # gonna have trouble if this is the case!
	$qname =~ s/ /_/g;
	while( my $sbjct = $report -> nextSbjct ) {
	    my( $sname, $st, $en ) = $sbjct -> name() =~ /^(\S+)\/(\d+)-(\d+)\s+/;
	    while( my $hsp = $sbjct->nextHSP ) {
		my $slength = $hsp->subject->end - $hsp->subject->start;
		next unless( ($slength/$qlength > 0.90) and ($hsp->percent > 90) );
		my $start = $st + $hsp->subject->start - 1;
		my $end   = $st + $hsp->subject->end - 1;
		my $evalue = $hsp->P();

		push( @{$hits{$sname}}, { 'start' => $start, 
					  'end' => $end, 
					  'id' => $qname, 
					  'evalue' => $evalue } );

#		print "YES\t$sname/$start-$end\t$qname\t$evalue\n";
	    }
	}
	last if( $report -> _parseHeader == -1 );
	redo;
    }

    my %keep;
    foreach my $id ( keys %hits ) {
	my @regions = sort { $a->{'evalue'} <=> $b->{'evalue'} } @{$hits{$id}};
      REG: foreach my $region ( @regions ) {
	  foreach my $kept ( @{$keep{$id}} ) {
	      if( ( $region->{'start'} >= $kept->{'start'} and $region->{'end'} <= $kept->{'end'} ) or 
		  ( $region->{'start'} <= $kept->{'end'}   and $region->{'end'} >= $kept->{'end'} ) or 
		  ( $region->{'start'} <= $kept->{'start'} and $region->{'end'} >= $kept->{'start'} ) ) {
		  next REG;
	      }
	  }
	  push( @{$keep{$id}}, $region );
      }
    }
    
    foreach my $id ( keys %keep ) {
	foreach my $reg ( @{$keep{$id}} ) {
	    print "$id/", $reg->{'start'}, "-", $reg->{'end'}, "\t", $reg->{'id'}, "\t", $reg->{'evalue'}, "\n";
	}
    }
}
