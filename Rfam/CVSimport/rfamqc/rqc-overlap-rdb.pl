#!/usr/local/bin/perl -w

use strict;
use Rfam;
use Rfam::RfamAlign;
use Getopt::Long;

my( $quiet,
    $nolog,
    $local_fams,
    @ignore);

&GetOptions("i=s@" => \@ignore,
	    "q"    => \$quiet,
	    "l"    => \$local_fams,
	    "n!"   => \$nolog);


if( $#ARGV == -1 ) {
    print "pqc-overlap - finds the overlap between a Pfam family and the\n";
    print "      current Pfam database\n";
    print "USAGE: pqc-overlap <model-directory> <optional - families to compare against>\n";
    print "     If no families given, assummes all\n";
    print "OPTIONS\n";
    print "  -i <family> ignore this family (-i can occur multiple times)\n";
    print "  -q quiet running\n";
    print "  -n  no log file\n";
    print "  -l  look in current directory for target families\n";
    exit(1);
}


my $family_dir = shift; # family dir to use

if( ! defined $nolog ) {
    open(LOG,">$family_dir/overlap") || die "Could not open log file $family_dir/overlap - can use -n option $!";
}


my $db = Rfam::default_db();
my @families = $db->get_allacc();

my %ignore;
foreach my $ignorefam ( @ignore ) {
    print STDERR "ignoring $ignorefam\n";
    $ignore{$ignorefam} = 1;
}

my @overlap;
if( defined $quiet ) {
    @overlap = &compare_overlap_to_current( $family_dir, \@families );
} else {
    @overlap = &compare_overlap_to_current( $family_dir, \@families, \*STDERR );
}


foreach my $overlap ( @overlap ) {
    # lines look like seq_name:domain-start-end:domain-start-end

    my ($name,$dom1,$dom2) = split(/:/,$overlap);
    $dom1 =~ /([\w-]+)-(\d+)-(\d+)/;
    $dom1 = $1;
    my $start1 = $2;
    my $end1 = $3;

    $dom2 =~ /([\w-]+)-(\d+)-(\d+)/;
    $dom2 = $1;
    my $start2 = $2;
    my $end2 = $3;
	
    &errorout ("Sequence [$name] overlap $dom1/$start1-$end1 with $dom2/$start2-$end2\n");
}

open( SEED, "$family_dir/SEED" ) or die "can't find $family_dir/SEED\n";
my $seed = new Rfam::RfamAlign;
$seed -> read_stockholm( \*SEED );
my @list = $seed->each_seq();

for( my $seq = shift(@list); defined $seq; $seq = shift(@list) ) {
    foreach my $other ( @list ) {
	if( $seq->id ne $other->id ) {
	    next;
	}
	my( $s1, $s2, $e1, $e2 );
	
	if( $other->start() > $other->end() ) {
	    ( $s1, $e1 ) = ( $other->end, $other->start );
	}
	else {
	    ( $s1, $e1 ) = ( $other->start, $other->end );
	}
	if( $seq->start > $seq->end ) {
	    ( $s2, $e2 ) = ( $seq->end, $seq->start );
	}
	else {
	    ( $s2, $e2 ) = ( $seq->start, $seq->end );
	}

	if( ( $s1 >= $s2 and $e1 <= $e2 ) or 
	    ( $s1 <= $e2 and $e1 >= $e2 ) or 
	    ( $s1 <= $s2 and $e1 >= $s2 ) ) {
	    &errorout (sprintf("Internal overlap of %s/%d-%d to %s/%d-%d\n",$seq->id,$seq->start,$seq->end,$other->id,$other->start,$other->end));
	}
    }
}

    
sub compare_overlap_to_current {
    my $dir = shift;
    my $fams_ref = shift;
    my $report = shift;

    my @families = @{$fams_ref};

    open( FULL, "$dir/ALIGN" ) or die "can't find $dir/ALIGN\n";
    my $full = new Rfam::RfamAlign;
    $full -> read_stockholm( \*FULL );
    open( SEED, "$dir/SEED" ) or die "can't find $dir/SEED\n";
    my $seed = new Rfam::RfamAlign;
    $seed -> read_stockholm( \*SEED );

    my %hash;
    foreach my $seq ( $full->each_seq(), $seed->each_seq() ) {
	push( @{ $hash{$seq->id()} }, { 'start' => $seq->start(),
					'end'   => $seq->end() } );
    }

    my @arry;
   
    my @keys = keys %hash;
    my $rdb = Rfam::switchover_rdb();

    my $count = 0;

    foreach my $seq ( $rdb->get_AnnotSeqs(\@keys, ['seed', 'full']) ) {
	my @current_regs = sort { $a->from <=> $b->from } ($seq->eachAnnotatedRegion);
	foreach my $current_reg (@current_regs) {
	    foreach my $startstop ( @{ $hash{$current_reg->rfamseq_id()} } ) {
		my( $s1, $s2, $e1, $e2 );
		
		if( $startstop->{'start'} > $startstop->{'end'} ) {
		    ( $s1, $e1 ) = ( $startstop->{'end'}, $startstop->{'start'} );
		}
		else {
		    ( $s1, $e1 ) = ( $startstop->{'start'}, $startstop->{'end'} );
		}
		
		if( $current_reg->from > $current_reg->to ) {
		    ( $s2, $e2 ) = ( $current_reg->to, $current_reg->from );
		}
		else {
		    ( $s2, $e2 ) = ( $current_reg->from, $current_reg->to );
		}
		
		if( ( $s1 >= $s2 and $e1 <= $e2 ) or 
		    ( $s1 <= $e2 and $e1 >= $e2 ) or 
		    ( $s1 <= $s2 and $e1 >= $s2 ) ) {
		    unless( $ignore{$current_reg->accession} ) {
			push( @arry, sprintf( "%s:%s-%d-%d:%s-%d-%d", $current_reg->rfamseq_id(),$dir,$start,$stop, $current_reg->accession() , $current_reg->from(),$current_reg->to()));
			$count ++;
		    }
		}
	    }
	}
    }
    if( $report ) {
	print $report sprintf("Done Model %-25s - Found %d overlaps\n",$dir,$count);
    }
    
    return @arry;
}



sub errorout {
    my $mess = shift;

    print STDOUT $mess;

    if( ! defined $nolog ) {
	print LOG $mess;
    }
}





