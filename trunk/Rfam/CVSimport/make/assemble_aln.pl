#!/usr/local/bin/perl -w

# take several alignments aligned to the same model using cmalign, and
# reconstitute one big one using the RF lines

use strict;
use Rfam::RfamAlign;

my @alnfiles = @ARGV;

my $newaln = Rfam::RfamAlign -> new();

foreach my $alnfile ( @alnfiles ) {
    open( A, $alnfile ) or die;
    my $oldaln = Rfam::RfamAlign -> new();
    $oldaln -> read_stockholm( \*A );
#    $oldaln -> map_chars( '-', '.' );

    if( not $newaln->match_states() ) {
	# first aln file - just copy to bigaln
	$newaln->match_states( $oldaln->match_states() );
	$newaln->ss_cons( $oldaln->ss_cons() );
    }
    else {
	# we're adding to an alignment
	my @newrf = split( //, $newaln->match_states() );
	my @oldrf = split( //, $oldaln->match_states() );

	my( $new_lastm, $new_nextm ); # save the position of the next and last match 
	my( $old_lastm, $old_nextm ); # state so we can decide which wat to justify

	my $insdir;   # 1 for right, 0 for left, undef for check

	for( my $i=0; $i<@newrf; $i++ ) {
	    # look ahead for the next match states
	    for( my $j=$i; $j<@oldrf; $j++ ) {
		if( $oldrf[$j] ne '.' ) {
		    $old_nextm = $j;
		    last;
		}
	    }
	    for( my $j=$i; $j<@newrf; $j++ ) {
		if( $newrf[$j] ne '.' ) {
		    $new_nextm = $j;
		    last;
		}
	    }

#	    print "$i $newrf[$i] $oldrf[$i] $new_lastm $new_nextm $old_lastm $old_nextm ";

	    if( $newrf[$i] eq $oldrf[$i] ) {
		undef $insdir;
#		print "\n";
	    }
	    else {
		# we need to insert a column in one or other alignment
		# or deal with a completely missed match state :(

		if( $oldrf[$i] eq '.' ) { 
		    # we need an insert in newaln
		    # but do we insert at position i to left justify or after the 
		    # last match state to right justify? I think this comes down 
		    # to whether the position after the last match state or before
		    # the next match state has the most non-gap chars in the 
		    # alignment!

		    my $inspos;
		    if( not defined $insdir ) {
			my( $count1, $count2 ) = (0,0);
			foreach my $seq ( $oldaln -> each_seq() ) {
			    my @ary = split( //, $seq->seq );
			    $count1 ++ if( $ary[$old_nextm-1] =~ /\w/ );
			    $count2 ++ if( $ary[$old_lastm+1] =~ /\w/ );
			}
			if( $count1 > $count2 ) {
			    $insdir = 1;  # right justify
			}
			else {
			    $insdir = 0;  # left justify
			}
#			print "1=$count1 2=$count2 ";
		    }
		    
		    if( $insdir ) {
			$inspos = $new_lastm+1;
		    }
		    else {
			$inspos = $i;
		    }

#		    print "INSERT1 $insdir\n";
			
		    splice( @newrf, $inspos, 0, '.' );
		    $newaln->insert_column( $inspos, "." );
		}
		elsif( $newrf[$i] eq '.' ) { 
		    # we need an insert in aln
		    # same decision as above!
		    my $inspos;
		    if( not defined $insdir ) {
			my( $count1, $count2 ) = (0,0);
			foreach my $seq ( $newaln -> each_seq() ) {
			    my @ary = split( //, $seq->seq );
			    $count1 ++ if( $ary[$new_nextm-1] =~ /\w/ );
			    $count2 ++ if( $ary[$new_lastm+1] =~ /\w/ );
			}
			if( $count1 > $count2 ) {
			    $insdir = 1;  # right justify
			}
			else {
			    $insdir = 0;  # left justify
			}
#			print "1=$count1 2=$count2 ";
		    }
		    
		    if( $insdir ) {
			$inspos = $old_lastm+1;
		    }
		    else {
			$inspos = $i;
		    }

#		    print "INSERT2 $insdir\n";
		    splice( @oldrf, $inspos, 0, '.' );
		    $oldaln->insert_column( $inspos, "." );
		}
		else {
		    # we have a skipped match state
		    # look ahead for the match
		    for( my $j=1; $j<5; $j++ ) {
			if( $j==4 ) {
			    die "I can't match up your RF lines\n";
			}
			if( $newrf[$i+$j] eq $oldrf[$i] ) {
			    splice( @oldrf, $i, 0, '.' );
			    $oldaln->insert_column( $i, "-" );
			    last;
			}
			elsif( $newrf[$i] eq $oldrf[$i+$j] ) {
			    splice( @newrf, $i, 0, '.' );
			    $newaln->insert_column( $i, "-" );
			    last;
			}
		    }
		}
	    }
	    $new_lastm = $i if( $newrf[$i] =~ /\w/ );
	    $old_lastm = $i if( $oldrf[$i] =~ /\w/ );
	}
    }

    foreach my $seq ( $oldaln->each_seq() ) {
	$newaln -> add_seq( $seq );
    }

}

$newaln -> write_stockholm( \*STDOUT );
