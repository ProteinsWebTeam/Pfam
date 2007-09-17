#!/usr/local/bin/perl -w

use strict;
use Getopt::Long;
use IO::File;

use Rfam;
use Rfam::RfamAlign;
use CMResults;

use Bio::Tools::BPlite;

my( $agp, $con, $search, @add, $clean, $outfile, $help );
&GetOptions( "agp"     => \$agp,
	     "con"     => \$con,
	     "search"  => \$search,
	     "add=s@"  => \@add,
	     "clean=s" => \$clean,        # clean up a load of results files from previous run
	     "o=s"     => \$outfile,
	     "h"       => \$help,
	     );

my $file = shift;
my $rfamseqlist = '/lustre/pfam/rfam/production/rfamseq/CURRENT/embl_sv.txt';

sub help {
    print STDERR <<EOF;
Usage:  $0 [--agp|--con] [--search] [--add resultsfile] file

            --agp things do not work properly at the moment

EOF
}

if( $search and not $outfile ) {
    &help();
    print STDERR "You need to specify an outfile if you use the --search option\n";
    exit(1);
}

my %ignoreacc = (
#		 "AE008687" => 1,
#		 "AE008689" => 1,
#		 "AE008690" => 1,
#		 "BA000006" => 1,
#		 "AE000521" => 1,
		 );


my %rfamseq;
open( L, $rfamseqlist ) or die;
while(<L>) {
    if( /^(\S+)\.(\d+)/ ) {
	$rfamseq{$1} = $2;
    }
}
close L;

my $rdb = Rfam::switchover_rdb();

my %search;
my $ignore;
open( E, $file ) or die "can't find your file [$file]\n";
my $seqs;
if( $agp ) {
    $seqs = read_agp( \*E );
}
elsif( $con ) {
    $seqs = read_cons( \*E );
}
else {
    my %seqs;
    open( F, $file ) or die;
    while(<F>) {
	if( /^(\S+)\.(\d+)[\.\/](\d+)[\.\-](\d+)/ ) {
#	    print "$1 $2 $3 $4\n";
	    push( @{ $seqs{'UNKNOWN'}->{'list'} }, { 'id'      => $1,
						     'ver'     => $2,
						     'start'   => $3,
						     'end'     => $4 } );
	}
    }
    close F;
    $seqs = \%seqs;
}

my %annseq;
if( @add ) {
    foreach my $file ( @add ) {
	open( F, $file ) or die;
	while(<F>) {
	    if( my( $sv, $st, $en, $rfacc, $modst, $moden, $bits, $rfid ) =
		/(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/ ) {
		
		unless( $annseq{$sv} ) {
		    my $annseq = Rfam::AnnotatedSequence->new();
		    $annseq{$sv} = $annseq;
		}
		$annseq{$sv}->addAnnotatedRegion( Rfam::RfamRegion->new( '-RFAM_ACCESSION' => $rfacc,
									 '-RFAM_ID' => $rfid,
									 '-SEQ_ID' => $sv,
									 '-FROM' => $st,
									 '-TO' => $en,
									 '-BITS' => $bits )
					     );
	    }
	}
    }
}

foreach my $ctg ( keys %{$seqs} ) {
    next if( exists $ignoreacc{ $ctg } );
    if( not $search ) {
	print "AC   $ctg;\n";
	print "DE   ", $seqs->{$ctg}->{'DE'}, "\n" if( $seqs->{$ctg}->{'DE'} );
	print "OS   ", $seqs->{$ctg}->{'OS'}, "\n" if( $seqs->{$ctg}->{'OS'} );
	print "OC   ", $seqs->{$ctg}->{'OC'}, "\n" if( $seqs->{$ctg}->{'OC'} );
    }

    foreach my $seq ( @{ $seqs->{$ctg}->{'list'} } ) {
	my( $id, $ver, $st, $en ) = ( $seq->{'id'}, $seq->{'ver'}, $seq->{'start'}, $seq->{'end'} );

	unless( @add ) {
	    if( not exists $rfamseq{$id} ) {
		print "$id\.$ver/$st-$en not found in rfamseq\n" unless $clean;
		next;
	    }

	    if( $ver != $rfamseq{$id} ) {
#	        my( $st2, $en2 ) = &mapseq( $id, $ver, $st, $en );
#	        if( not $en2 ) {
	    	print "$id\.$ver/$st-$en version mismatch\n" unless $clean;
	    	next;
#	        }
#	        ( $st, $en ) = ( $st2, $en2 );
	    }
	}
	print_hits( "$id.$ver", $st, $en );
    }
    print "\/\/\n";
}


########

sub print_hits {
    my( $sv, $constart, $conend ) = @_;
#    print STDERR "getting $sv\n";
    my( $seqid ) = $sv =~ /^(\S+)\.\d+/;
    print "$sv\n";
    my @annseqs;
    if( $annseq{$sv} ) {
	push( @annseqs, $annseq{$sv} );
    }
    else {
      GET: {
	  eval {
	      @annseqs = $rdb -> get_AnnotSeqs( [$sv], ['full'] );
#	      print "@annseqs\n";
	  };
	  if( $@ ) {
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

		printf( "RF   %-26s %-10s %-16s %7s\n", $reg->rfamseq_id()."/".$reg->from."-".$reg->to, $reg->accession, $reg->id, sprintf( "%.2f", $reg->bits_score ) );
	    }
	}
    }
}


sub mapseq {
    my( $id, $ver, $st, $en ) = @_;
    my $fh = IO::File -> new();

    my $pfopt = "-a";
  FETCH: {
      open( O, ">$$.sub.fa" ) or die;
#    print STDERR "end $en\n";
      $fh -> open( "pfetch $pfopt $id\.$ver:$st-$en -n $id\.$ver/$st-$en|" ) or die;
      while(<$fh>) {
	  if( /end is greater than sequence length.*?(\d+)/ ) {
	      $en = $1;
	      redo FETCH;
	  }
	  elsif( /no match/ ) {
	      if( not $pfopt ) {
		  warn "failed to pfetch $id\.$ver/$st-$en";
		  return 0;
	      }
	      $pfopt = "";
	      redo FETCH;
	  }
	  else {
	      print O $_;
	  }
      }
  }
    system "pfetch -a $id\.$rfamseq{$id} -n $id\.$rfamseq{$id} > $$.whole.fa" and die;
			
    system "formatdb -i $$.whole.fa -p F" and die;
    system "blastall -F F -d $$.whole.fa -i $$.sub.fa -p blastn -e 0.000001 > $$.blast" and die;

    open( BLAST, "$$.blast" ) or die "can't open $$.blast";
    my $report = new Bio::Tools::BPlite( '-fh' => \*BLAST );
    my $best;
    {
	my $querylength = $report->qlength;
	while( my $sbjct = $report->nextSbjct ) {
	    while( my $hsp = $sbjct->nextHSP ) {
		if( (not $best or $hsp->bits >= $best->bits) ) {
		    $best = $hsp;
		}
	    }
	}
        last if ($report->_parseHeader == -1);
	redo;
    }
    close BLAST;

    if( $best ) {
	print STDERR "mapping $id\.$ver/$st-$en to $id\.$rfamseq{$id}/", $best->hit->start, "-", $best->hit->end, "\n";
	return( $st, $en );
    }
}

sub read_agp {
    my $fh = shift;
    my %seqs;
    while(<$fh>) {
	if( /^\S+\s+\d+\s+\d+\s+\d+\s+\S+\s+(\S+)\.(\d+)\s+(\d+)\s+(\d+)/ ) {
	    my( $id, $ver, $st, $en ) = ( $1, $2, $3, $4 );
	    push( @{ $seqs{'UNKNOWN'}->{'list'} }, { 'id'      => $id,
						     'ver'     => $ver,
						     'start'   => $st,
						     'end'     => $en } );
	}
    }
    return \%seqs;
}

sub read_cons {
    my $fh = shift;
    my( %seqs, $acc );
    while(<$fh>) {
	if( /^AC\s+(\S+)\;/ ) {
	    $acc = $1;
	}
	if( /^(DE)\s+(.*)/ or /^(OC)\s+(.*)/ or /^(OS)\s+(.*)/ ) {
	    $seqs{$acc}->{$1} .= "$2 ";
	}
	if( my( $stuff ) = /^CO\s+(.*)/ ) {
	    while( $stuff =~ /([A-Z0-9]+)\.(\d+)\:(\d+)\.\.(\d+)/g ) {
		my( $id, $ver, $st, $en ) = ( $1, $2, $3, $4 );
		push( @{ $seqs{$acc}->{'list'} }, { 'id'      => $id,
						    'ver'     => $ver,
						    'start'   => $st,
						    'end'     => $en } );
	    }
	}
    }
    return \%seqs;
}
