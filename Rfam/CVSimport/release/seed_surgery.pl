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
    $queue,
    $verbose );

&GetOptions( "noaction" => \$noaction,
	     "dir"      => \$dir,
	     "noclean"  => \$noclean,
	     "rename"   => \$rename,
	     "q=s"      => \$queue,
	     "v"        => \$verbose );

if( $dir ) {
    chdir $dir or die "can't chdir to $dir\n";
}

my $inx = Bio::Index::Fasta->new( $Rfam::rfamseq_current_inx );

END { undef $inx; }  # stop bizarre seq faults;

my %cache;
my @list;
my $db = Rfam::default_db();
@list = @ARGV or @list = $db->get_allacc();

foreach my $acc ( @list ) {
    print STDERR "Family $acc:\n";
    my( $seq_change, $cosmetic_change );
    my $aln = new Rfam::RfamAlign;
    open( SEED, "$acc/SEED" ) or die;
    $aln -> read_stockholm( \*SEED );

    my $newaln = Rfam::RfamAlign -> new();
    if( $aln->ss_cons ) {
	$newaln->ss_cons( $aln->ss_cons );
    }
    if( $aln->match_states ) {
	$newaln->match_states( $aln->match_states );
    }

    foreach my $oldseq ( $aln -> each_seq() ) {
	my( $oldstart, $oldend ) = ( $oldseq->start, $oldseq->end );

	# $seq is the sequence we will add to the new aln object -
	# we can't simply manipulate the existing sequences because
	# everything goes very bad when we change the id of a sequence
	# object that is in an alignment (Que?)
	my $seq = Bio::LocatableSeq -> new( '-id'    => $oldseq->id(),
					    '-start' => $oldseq->start(),
					    '-end'   => $oldseq->end(),
					    '-seq'   => $oldseq->seq() );

	if( $seq->id() =~ /^(\S+)\.(\d+)$/ ) {
	    $seq->accession_number($1);
	    $seq->version($2);
	}
	else {
	    $seq->accession_number( $seq->id() );
	}
	
	if( my $change = &t_to_u( $seq ) ) {
	    print STDERR $seq->id, " T_TO_U\n";
	    $cosmetic_change = 1;
	}

	if( my $newsv = &new_sv( $seq ) ) {         # accession exists in database
	    print STDERR $seq->id, " ACCESSION_OK\n" if $verbose;
	    my( $newstart, $newend ) = &subseq_is_unchanged( $seq );
	    if( $newend ) { 
		if( $newstart != $seq->start or $newend != $seq->end ) {
		    print STDERR $seq->id, " START_END_CHANGE\n" if $verbose;		    
		    $seq->start( $newstart );
		    $seq->end( $newend );
		    $cosmetic_change = 1;
		}
		else {
		    print STDERR $seq->id, " SEQ_OK\n" if $verbose;
		}

		if( $seq->version() and $seq->version() == $newsv ) {   
		    # leave well alone
		}
		else {                              # sv is different
		    print STDERR $seq->id, " NEW_VERSION $newsv\n" if $verbose;
		    $seq->version( $newsv );
		    $seq->id( $seq->accession_number.".".$newsv );
		    $cosmetic_change = 1;
		}
	    }
	    else {   # the sequence has changed (hopefully only slightly)
		print STDERR $seq->id, " SEQ_CHANGE\n" if $verbose;
	    }
	}
	else {       # we've got to map to a new accession
	    print STDERR $seq->id, " DELETE\n";
	    print STDERR "\n" if $verbose;
	    $seq_change = 1;
	    next;
	}

	print STDERR sprintf( "%-28s%-28s\n", $oldseq->id."/".$oldseq->start."-".$oldseq->end, $seq->id."/".$seq->start."-".$seq->end ) if $verbose;
	print STDERR "\n" if $verbose;
	$newaln -> add_seq( $seq );

    }

    if( !$noaction and ( $cosmetic_change or $seq_change ) ) {
	open( NEW, ">$acc/SEEDNEW" ) or die "can't write to $acc/SEEDNEW";
	$newaln -> write_stockholm( \*NEW );
	close NEW;

	rename( "$acc/SEED", "$acc/SEEDOLD" ) or die "can't rename SEED to SEEDOLD";
	rename( "$acc/SEEDNEW", "$acc/SEED" ) or die "can't rename SEEDNEW to SEED";

	if( $seq_change ) {
	    print STDERR "Family $acc: The alignment has changed - you must rebuild the family\n\n";
	}
	else {
	    print STDERR "Family $acc: Cosmetic changes - no rebuild needed\n\n";
	}
    }
}


###############

sub t_to_u {
    my $seq = shift;
    my $curstr = $seq -> seq();
    my $change;
    if( $curstr =~ tr/Tt/Uu/ ) {    # "It's RNA dammit" (SRE)
	$seq->seq( $curstr );
	$change = 1;
    }
    return $change;
}

sub get_seq {
    my $acc = shift;
    eval {
	$cache{$acc} = $inx->fetch( $acc ) unless exists $cache{$acc};
    };
    if( $@ or not $cache{$acc} ) {
#	warn "$acc not found in your database: $@\n";
	return undef;
    }
    return $cache{$acc};
}

sub new_sv {
    my $seq    = shift;
    my $newseq = &get_seq( $seq->accession_number() );
    if( $newseq ) {
	my( $version ) = $newseq->id() =~ /^\S+\.(\d+)/;
	return $version if( $version );
    }
    return undef;
}

sub subseq_is_unchanged {
    my $seq    = shift;
    my $newseq = &get_seq( $seq->accession_number() );

    my $reverse;
    my( $oldstr, $oldstr2 );

    $seq->alphabet( 'rna' );
    my $revseq = $seq->revcom();

    if( $seq->start > $seq->end ) {   # we did have a reverse strand hit
	$oldstr  = $revseq->seq();    # so we'll try that first
	$oldstr2 = $seq->seq();
	$reverse = 1;
    }
    else {                            # try forward strand hit first
	$oldstr  = $seq->seq();
	$oldstr2 = $revseq->seq();
    }

    $oldstr =~ s/[\-\.]//g;
    $oldstr =~ tr/Uu/Tt/;

    $oldstr2 =~ s/[\-\.]//g;
    $oldstr2 =~ tr/Uu/Tt/;

    if( $newseq->seq() =~ /^(\w*)$oldstr(\w*)$/i ) {
	my $start = length($1)+1;
	my $end   = length($1)+length($oldstr);
#	print STDERR $seq->id, " HIT_SAME_STRAND\n";
	if( $reverse ) {
	    return $end, $start;
	}
	else {
	    return $start, $end;
	}
    }
    elsif( $newseq->seq() =~ /^(\w*)$oldstr2(\w*)$/ ) {  # now try the opposite strand
	my $start = length($1)+length($oldstr2);
	my $end   = length($1)+1;
#	print STDERR $seq->id, " HIT_OPPOSITE_STRAND\n";
	if( $reverse ) {
	    return $end, $start;
	}
	else {
	    return $start, $end;
	}
    }
    else {
	return undef;
    }
}

##############


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
