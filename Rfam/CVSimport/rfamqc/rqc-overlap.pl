#!/usr/local/bin/perl

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

open( SEED, "$family_dir/SEED" ) or die;
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

    open( FULL, "$dir/ALIGN" ) or die;
    my $full = new Rfam::RfamAlign;
    $full -> read_stockholm( \*FULL );
    open( SEED, "$dir/SEED" ) or die;
    my $seed = new Rfam::RfamAlign;
    $seed -> read_stockholm( \*SEED );

    my %hash;
    foreach my $seq ( $full->each_seq(), $seed->each_seq() ) {
	$hash{$seq->id()} .= sprintf("%d-%d:",$seq->start(),$seq->end());
    }

    my @arry;

    FAMILY: foreach my $family ( @families ) {
	my $count = 0;

	open( FULL, "$Rfam::current_dir/$family/ALIGN" ) or die;
	my $full = new Rfam::RfamAlign;
	$full -> read_stockholm( \*FULL );
	open( SEED, "$Rfam::current_dir/$family/SEED" ) or die;
	my $seed = new Rfam::RfamAlign;
	$seed -> read_stockholm( \*SEED );

	foreach my $seq ( $full->each_seq(), $seed->each_seq() ) {
	    if( $hash{$seq->id()} ) {
		$_ = $hash{$seq->id()};
		chop; # trailing :
		my @startstop = split(/:/);
		foreach my $startstop ( @startstop) {
		    my( $start, $stop ) = split( /-/, $startstop );
		    my( $s1, $s2, $e1, $e2 );

		    if( $start > $stop ) {
			( $s1, $e1 ) = ( $stop, $start );
		    }
		    else {
			( $s1, $e1 ) = ( $start, $stop );
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
			push( @arry, sprintf( "%s:%s-%d-%d:%s-%d-%d", $seq->id(),$dir,$start,$stop,$family,$seq->start(),$seq->end()));
			$count++;
		    }
		}
	    }
	}
	if( $report ) {
	    print $report sprintf("Done Model %-25s - Found %d overlaps\n",$family,$count);
	}

	$family = undef; # free Model
    } # end of for each family

    return @arry;
}



sub errorout {
    my $mess = shift;

    print STDOUT $mess;

    if( ! defined $nolog ) {
	print LOG $mess;
    }
}





