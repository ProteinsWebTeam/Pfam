#!/usr/local/bin/perl -w

# performs seed surgery on Rfam families at rfamseq update time
# (also handy during familiy building!)

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

use Bio::SeqIO;
use Bio::Index::Fasta;
use Bio::SearchIO;
use Rfam;
use Rfam::RfamAlign;

my( $noaction,
    $dir,
    $noclean,
    $rename,
    $queue );

&GetOptions( "noaction" => \$noaction,
	     "dir"      => \$dir,
	     "noclean"  => \$noclean,
	     "rename"   => \$rename,
	     "q=s"      => \$queue );

if( $dir ) {
    chdir $dir or die "can't chdir to $dir\n";
}
my $newinx = Bio::Index::Fasta->new( $Rfam::rfamseq_new_inx );
my %cache;
my @list;
my $db = Rfam::default_db();
@list = @ARGV or @list = $db->get_allacc();


#open( LOG, ">log" ) or die "can't write to $dir/log\n";

foreach my $acc ( @list ) {
    my $changed;
    my $aln = new Rfam::RfamAlign;
    open( SEED, "$acc/SEED" ) or die;
    $aln -> read_stockholm( \*SEED );
    my $newaln = Rfam::RfamAlign->new();
    $newaln->ss_cons( $aln->ss_cons );

    foreach my $seq ( $aln -> each_seq() ) {
	my $addseq = Bio::LocatableSeq -> new( -id    => $seq->id,
					       -start => $seq->start,
					       -end   => $seq->end,
					       -seq   => $seq->seq );
	my $curstr = $seq -> seq();
	if( $curstr =~ tr/Tt/Uu/ ) {    # "It's RNA dammit" (SRE)
	    printf( "%s   %-10s%10d%10d   ", $acc, $seq->id,$seq->start,$seq->end );
	    print "T_TO_U\n";
	    $addseq -> seq( $curstr );
	    $changed = 1;
	}
	$curstr =~ s/[\.\-]//g;
	$curstr = uc( $curstr );

	if( !$rename and my $newseq = &return_seq( $newinx, $seq->id, $seq->start, $seq->end ) ) {
	    my $newstr = $newseq -> seq;
	    $newstr =~ tr/Ttu/UUU/; 
	    $newstr = uc( $newstr );
	    if( $newstr ne $curstr ) {
		print "\n$curstr\n$newstr\n";
		printf( "%s   %-10s%10d%10d   %-10s%10d%10d   ", $acc, $seq->id, $seq->start, $seq->end, $seq->id, $seq->start, $seq->end );
		my $fixed = &find_match( $newinx, $addseq );
		if( $fixed == -1 ) {
		    print "RENUMBER\tFIXED\n";
		    $changed = 1;
		}
		elsif( $fixed ) {
		    print $fixed, "_MISMATCH\tFIXED\n";
		    $changed = 1;
		}
		else {
		    print "MISMATCH\tNOTFIXED\n";
		    $changed = 1;
		}
	    }
	}
	else {
	    if( $rename ) {
		printf( "%s   %-10s%10d%10d     ", $acc, $seq->id, $seq->start, $seq->end );
		my $fixed = &find_match( $newinx, $addseq, 1 );
		if( $fixed == -1 ) {
		    print "RENUMBER\tFIXED\n";
		    $changed = 1;
		}
		elsif( $fixed ) {
		    print $fixed, "_MISMATCH\tFIXED\n";
		    $changed = 1;
		}
		else {
		    print "MISMATCH\tNOTFIXED\n";
		    $changed = 1;
		    next;
		}		
	    }
	    else {
		printf( "%s   %-10s%10d%10d     ", $acc, $seq->id, $seq->start, $seq->end );
#		$aln -> remove_seq( $seq );
		print "DELETE\tFIXED\n";
		$changed = 1;
		next;
	    }
	}
	$newaln -> add_seq( $addseq );
    }

#    if( $newaln -> allgaps_columns_removed ) {
#	printf( "%s   GAPS_REMOVED\n", $acc );
#	$changed = 1;
#    }

    if( $changed and not $noaction ) {
	open( NEW, ">$acc/SEEDNEW" ) or die "can't write to $acc/SEEDNEW";
	$newaln -> write_stockholm( \*NEW );
	close NEW;

	rename( "$acc/SEED", "$acc/SEEDOLD" ) or die "can't rename SEED to SEEDOLD";
	rename( "$acc/SEEDNEW", "$acc/SEED" ) or die "can't rename SEEDNEW to SEED";
    }
}

undef $newinx;


sub return_seq {
    my $inx  = shift;
    my $id   = shift;
    my $from = shift;
    my $to   = shift;

    eval {
	$cache{$id} = $inx -> fetch( $id ) unless exists $cache{$id};
    };
    if( $@ or not $cache{$id} ) {
	warn "$id not found in your database: $@\n";
	return undef;
    }
    my $truncseq;
    if( not $from and not $to ) {
	$truncseq = $cache{$id};
    }
    else {
	if( $to > $cache{$id}->length ) {
	    $to = $cache{$id}->length;
	}
	eval {
	    if( $from > $to ) {
		$truncseq = $cache{$id} -> trunc( $to, $from );
		$truncseq = $truncseq -> revcom;
	    }
	    else {
		$truncseq = $cache{$id} -> trunc( $from, $to );
	    }
	};
	if( $@ or not $truncseq ) {
	    warn "$id: $from-$to are not reasonable bounds\n";
	    return 0;
	}
    }
    return $truncseq;
}


sub find_match {
    my $newinx  = shift;
    my $seq     = shift;
    my $rfamseq = shift;
    my $id      = $seq -> id;

    open( OUT, ">$$.old.fa" ) or die "can't write to $$.old.fa";
    $seq -> display_id( $seq->id."/".$seq->start."-".$seq->end );
    my $out = new Bio::SeqIO( '-fh' => \*OUT, '-format' => 'Fasta' );
    $out -> write_seq( $seq );
    close OUT;
    $seq -> display_id( $id );

    my $seqdb;
    if( $rfamseq ) {
	my @blastdbs = glob( "$Rfam::rfamseq_current_dir/*.fa" );
	if( -s "$$.blast" ) {
	    unlink( "$$.blast" ) or die;
	}
	foreach my $blastdb ( @blastdbs ) {
	    system "blastall -W 30 -F F -d $blastdb -i $$.old.fa -p blastn -v 5 -b 5 >> $$.blast" and die "can't run blastall";
	}
    }
    else {
	open( OUT, ">$$.new.fa" ) or die "can't write to $$.new.fa";
	my $out = new Bio::SeqIO( '-fh' => \*OUT, '-format' => 'Fasta' );
	my $newseq = $newinx -> fetch( $seq -> id );
	$out -> write_seq( $newseq );
	close OUT;

	system "formatdb -i $$.new.fa -p F" and die "can't formatdb $$.new.fa";
	system "blastall -W8 -F F -d $$.new.fa -i $$.old.fa -p blastn > $$.blast" and die "can't run blastall";
    }

    my( $best, $newid );
    my $multiple = Bio::SearchIO->new( -file => "$$.blast", '-format' => 'Blast' );
    my $querylength;
    while( my $result = $multiple->next_result ) {
	$querylength = $result->query_length unless $querylength;
	while( my $hit = $result->next_hit ) {
	    while( my $hsp = $hit->next_hsp ) {
		if( not $best or $hsp->bits >= $best->bits ) {
		    $best = $hsp;
		    $newid = $hit->name;
		}
	    }
	}
    }

    if( not $best ) {
	warn "can't find a matching sequence\t";
	return 0;
    }

    my $fixed;
    $seq->id( $newid );
    my( $hitstart, $hitend ) = ( $best->start('hit'), $best->end('hit') );
    if( $best->strand('query') != $best->strand('hit') ) {
	( $hitstart, $hitend ) = ( $hitend, $hitstart );
    }

    if( $best -> num_identical == $querylength ) {
	$seq -> start( $hitstart );
	$seq -> end( $hitend );
	$fixed = -1;
    }
    elsif( $best->length('hit') > $best->length('query') ) {
	# inserts in subject sequence -- can't deal easily
	$fixed = 0;
    }
    else {
	# map old -> new

	my @oldgap = split( //, $seq -> seq );
	my @newgap;
	my $subaln = $best -> hit_string;
	$subaln =~ tr/acgt-/ACGU\./;
	my @subgap = split( //, $subaln );
	my $j=0;
	for( my $k=1; $k<$best->start('query'); $k++ ) {
	    unshift( @subgap, "." );
	}
	for( my $i=0; $i<@oldgap; $i++ ) {
	    if( $oldgap[$i] !~ /[ACTUGNactugn]/ ) {
		push( @newgap, "." );
	    }
	    elsif( $j>@subgap ) {
		push( @newgap, "." );
	    }
	    else {
		push( @newgap, $subgap[$j] );
		$j++
	    }
	}

	$seq -> start( $hitstart );
	$seq -> end( $hitend );
	$seq -> seq( join( "", @newgap ) );
	$fixed = ( $querylength - $best->num_identical );

#	print "\n", join( "", @oldgap ), "\n", join( "", @newgap ), "\n";
    }

    unlink( "$$.new.fa", "$$.old.fa", "$$.new.fa.nhr", "$$.new.fa.nin", "$$.new.fa.nsq", "$$.blast" ) or die "can't cleanup" unless $noclean;

    return $fixed;
}
