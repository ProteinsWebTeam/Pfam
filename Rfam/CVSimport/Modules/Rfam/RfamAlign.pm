#
# RfamAlign.pm
#
# Inherits from Bioperl's SimpleAlign.pm and adds a few things.
# Should live with Rfam code outside of bioperl tree in an attempt
# to leave the two separately upgradable.
#

=head1 NAME

Rfam::RfamAlign

=head1 SYNOPSIS

    use Bio::Rfam::RfamAlign;

    $aln = new Bio::Rfam::RfamAlign->new;
    eval {
        $aln->read_stockholm( $fh );
    };
    $@ and do { $i->dont_know( $what ) };;

=head1 DESCRIPTION

Inherits from Bioperl's Bio::SimpleAlign and adds things to deal
correctly with secondary structure markup etc from stockholm format.

=cut

package Rfam::RfamAlign;
use vars qw( $AUTOLOAD @ISA @EXPORT_OK );
use strict;

use Bio::SimpleAlign;
use Rfam::SS;

@ISA = qw( Bio::SimpleAlign );


# new is probably not needed at the moment, but we have it
# here to make the SS_CONS and MATCH_STATES explicit and
# documented

sub new {
    my $caller = shift;
    my $class  = ref( $caller ) || $caller;
    my $self   = $class -> SUPER::new();
    $self -> { 'SS_CONS' }      = undef;
    $self -> { 'MATCH_STATES' } = undef;
    $self -> { 'CONSENSUS' }    = undef;
    return $self;
}


sub consensus {
    my $self = shift;
    if( not $self -> { 'CONSENSUS' } ) {
	$self -> { 'CONSENSUS' } = $self -> _compute_consensus();
    }
    return $self -> { 'CONSENSUS' };
}

sub ss_cons {
    my $self = shift;
    $self -> { 'SS_CONS' } = shift if @_;
    return $self -> { 'SS_CONS' };
}

sub match_states {
    my $self    = shift;
    $self -> { 'MATCH_STATES' } = shift if @_;
    return $self -> { 'MATCH_STATES' };
}


# SimpleAlign docs suggest this is implemented therein but it aint!
# Nicked from Pfam::AlignPfam

# altered to act on $self and return list of columns removed

sub allgaps_columns_removed {
    my ($self) = @_;
    my (@columnlist, %mymap);
    my @index_list = (0..$self->length-1);

    foreach my $seq ($self->each_seq) {
        my @ary = split( //, $seq->seq() );
        foreach my $el (grep { $ary[$_] ne '.' and $ary[$_] ne '-' }  (@index_list)) {
            $mymap{ $el } = 1;
        }
    }

    my @sortedgappositions = sort { $b <=> $a } grep { not defined( $mymap{$_}) }  (@index_list);
    my @sort2 = map { $_ + 1 } @sortedgappositions;    # removeColumn index starts at 1 not 0
    $self -> ss_cons -> removeColumn( @sort2 );    

    if( $self -> match_states ) {
	my @newrf = split( //, $self -> match_states );
	foreach my $gappos (@sortedgappositions) {
	    splice @newrf, $gappos, 1;
	}
	$self -> match_states( join( "", @newrf ) );
    }

    foreach my $seq ($self->each_seq) {
	my @newseq = split( //, $seq->seq() );

        foreach my $gappos (@sortedgappositions) {
            splice @newseq, $gappos, 1;
        }

	$seq -> seq( join ( "", @newseq ) );
    }

    return @sortedgappositions;
}



sub trimmed_alignment {
    my ($self, $start, $end) = @_;

    if (not defined($start) and not defined($end)) {
        $self->throw("The desired extent of the alignment has not been given in any way");
    }
    elsif (not defined($start)) {
        $start = 1;
    }
    elsif (not defined($end)) {
        $end = $self->length();
    }
    
    my @rf = split( //, $self -> match_states );

    my @junkrf = splice( @rf, 0, $start-1 );
    my $newrf  = join( '', splice( @rf, 0, $end-$start+1 ) );
    $self -> match_states( $newrf );

    if( $start > 1 ) {
	$self->ss_cons->removeColumn( 1..$start-1 );
    }
    if( $end < $self->length_aln() ) {
	$self->ss_cons->removeColumn( $end+1..$self->length() );
    }

    foreach my $seq ($self->each_seq()) {
        my @residues = split( //, $seq->seq() );
        my @discardedleft = splice( @residues, 0, $start-1 );

        # now, if we splice the first ($end - $start + 1) residues, then that is 
        # what we are interested in. @residues will be left with what is discarded right.
 
        $seq -> seq( join( '', splice( @residues, 0, $end-$start+1 ) ) );

	my $newstart = $seq->start();
	my $newend   = $seq->end();

        foreach my $char (@discardedleft) {
            if ($char ne '-' and $char ne '.') {
		if( $seq->start() > $seq->end() ) { 
		    $newstart--;
		}
		else {
		    $newstart++;
		}
            }
        }
        foreach my $char (@residues) {
            if ($char ne '-' and $char ne '.') {
		if( $seq->end() < $seq->start() ) { 
		    $newend++;
		}
		else {
		    $newend--;
		}
            }
        }

	$seq -> start( $newstart );
	$seq -> end( $newend );

        # we may be left with just gaps in the sequence; if this is the case
        # then remove it

        if ($newend == $newstart) {
	    $seq -> remove_seq( $seq );
        }
    }

    return 1;
}


sub read_stockholm {
    my $self = shift;
    my $in = shift;

    my( $ss_cons, 
	$match_states,
	%align,
	%c2name,
	$count );

    while( <$in> ) {
        /^\# STOCKHOLM/ && next;

        /^\/\// && do {
            # we have reached the end of the entry
            last;
        };
        /^\#=GC\s+SS_cons\s+(.+)/ && do {
            $ss_cons .= $1; 
            next;
        };
        /^\#=GC\s+RF\s+(.+)/ && do {
            $match_states .= $1;
            next;
        };
        
        /^([^\#]\S+)\s+([A-Za-z\.\-]+)\s*/ && do {      
            my $name = $1;
            my $seq = $2;
            
            if( ! defined $align{$name}  ) {
                $count++;
                $c2name{$count} = $name;
            }
            
            $align{$name} .= $seq;
            next;
        };

        # Blank line? fine. Comment? fine. Anything else? Forget it

        /\w/ && !/^\#/ && do {
            $self->throw("Line [$_] is not valid stockholm format");
        };
    }

    # ok... now we can make the sequences

    $count = 0;
    foreach my $no ( sort { $a <=> $b } keys %c2name ) {
        my $name = $c2name{$no};
	my( $seqname,
	    $start,
	    $end );
	
        if( $name =~ /(\S+)\/(\d+)-(\d+)/ ) {
            $seqname = $1;
            $start = $2;
            $end = $3;
        } else {
            $seqname=$name;
            $start = 1;
            $end = length($align{$name});
        }

        my $seq = new Bio::LocatableSeq( '-seq'   => $align{$name},
					 '-id'    => $seqname,
					 '-start' => $start,
					 '-end'   => $end, 
					 '-type'  => 'aligned'
					 );

        $self -> add_seq($seq);
        $count++;
    }

    if( $ss_cons ) {
	my $ss = new Rfam::SS;
	$ss -> parseInfernalString( $ss_cons );
	$self -> ss_cons( $ss );
    }
    if( $match_states ) {
	$self -> match_states( $match_states );
    } 

    return $count; 
}


sub write_structure_ps {
    my $self = shift;
    my $out  = shift;

    my $newaln = new Rfam::RfamAlign;
    my $conseq = new Bio::LocatableSeq( '-id'  => "Seq_cons",
					'-start' => 1,
					'-end' => $self->length(),
					'-seq' => $self->consensus() );

    $newaln -> add_seq( $conseq );
    $newaln -> ss_cons( $self->ss_cons );
    $newaln -> allgaps_columns_removed();

    # get the colours from Mhairi's webpage things
    my( %colours, $count );
    my $cssfile = "/nfs/WWW/htdocs/Software/Rfam/rfam_align.css";
    if( -s $cssfile ) {
	open( C, $cssfile ) or die;
	while(<C>) {
	    if( /\#\S+\s+\{\s*background-color\:\s*\#(\S{2})(\S{2})(\S{2})\;\s*\}/ ) {
		$count++;
		my $r = sprintf("%.2f", hex($1)/256);
		my $g = sprintf("%.2f", hex($2)/256);
		my $b = sprintf("%.2f", hex($3)/256);
		$colours{$count} = "$r $g $b";
#		print STDERR "$count $r $g $b\n";
	    }
	}
	close C;
    }
    # or fall back on some defaults
    if( not exists $colours{1} ) {
	%colours = ( 1 => "1.00 0.60 0.60",
		     2 => "0.60 0.60 1.00",
		     3 => "0.60 1.00 0.60",
		     4 => "1.00 0.60 0.00",
		     5 => "0.60 1.00 1.00",
		     6 => "0.59 0.75 0.75",
		     7 => "1.00 1.00 0.60",
		     8 => "1.00 0.20 1.00",
		     9 => "0.20 1.00 1.00",
		     10 => "1.00 1.00 0.20",
		     11 => "0.18 0.84 0.05",
		     12 => "0.95 0.74 0.97",
		     13 => "1.00 0.60 0.00",
		     14 => "0.72 0.31 0.20",
		     15 => "1.00 0.00 0.00",
		     16 => "1.00 0.80 0.60",
		     17 => "0.80 0.80 0.80",
		     18 => "0.80 0.20 0.40",
		     19 => "0.80 1.00 0.40",
		     20 => "1.00 0.80 0.40",
		     21 => "0.49 0.39 0.18",
		     );
    }

    my( $seq ) = $newaln -> each_seq();

    open( T, ">$$.rna" ) or die;
    print T ">$$.rna\n", $seq->seq(), "\n", $newaln->ss_cons->getViennaString(), "\n";
    close T;

    system "RNAplot < $$.rna > /dev/null" and die "can't run RNAplot";
    open( PS, "$$.rna_ss.ps" ) or die;
    while(<PS>) {
	if( /^\/drawpairs\s+true/ ) {
	    print $out $_;
	    print $out "/drawcolours true def  % Alex's hack to add structure colouring\n";
	    next;
	}
	if( /% draw the outline/ ) {
	    print $out <<EOF;

% Draw structure colours - Alex hacking around
drawcolours {
  12 setlinewidth
  
  colourranges { coor exch aload pop
  setrgbcolor    % stack: [coor] start end
  % delete one from start and end to get array index
  1 sub exch 1 sub exch
  2 copy pop     % stack: [coor] start end start
  sub 1 add      % stack: [coor] start range
  getinterval    % stack: [coor slice]
  dup            % stack: [coor slice] [coor slice]
  newpath
  0 get aload pop 0 0 360 arc
  {aload pop lineto} forall
  stroke
  } forall

  0.8 setlinewidth
} if

EOF
            print $out $_;
            next;
        }
        if( /^\/pairs \[/ ) {
	    print $out "\/colourranges \[\n";

	    my %colmap = %{ $newaln->ss_cons->column_colourmap };
	    my( $first, $last );
	    for( my $i=1; $i-1<=$newaln->ss_cons->length; $i++ ) {
		if( exists $colmap{$i} ) {
                    # print STDERR "$i $colmap{$i}\n";
                    if( !exists $colmap{$i-1} or $colmap{$i} != $colmap{$i-1} ) {
			print $out "\[$first $last $colours{$colmap{$first}}\]\n" if $first;
			$first = $last = $i;
		    }
                    else {
		        $last++;
		    }
                }
	        else {
		    print $out "\[$first $last $colours{$colmap{$first}}\]\n" if $first;
                    undef $first;
                }
            }  

            print $out "\] def\n";
	    print $out $_;
	    next;
	}
	if( /^drawbases/ ) {
	    print $out $_;
	    print $out "  0 setgray\n";
	    next;
	}
	if( /^drawoutline/ ) {
	    print $out $_;
	    print $out "  0.5 setgray\n";
	    next;
	}
	if( /^drawpairs/ ) {
	    print $out $_;
	    print $out "  0.5 setgray\n";
	    next;
	}
	if( /\[9 3.01\] 9 setdash/ ) {
	    print $out "  [5 12] 12 setdash\n";
	    next;
	}
	print $out $_;
    }
    close PS;
    unlink( "$$.rna", "$$.rna_ss.ps" ) or die;
}

sub write_stockholm {
    my $self  = shift;
    my $out   = shift;
    my $block = shift;
    $block = 50 if( not defined $block );
    $block = $self -> length if( not $block );

    my $maxn = $self->maxdisplayname_length() + 2;
    my $iter = $self->length/$block;
    print $out "\# STOCKHOLM 1.0\n\n";

    my $ss_str;
    eval {
	$ss_str = $self->ss_cons->getInfernalString();
    };
	
    for( my $i=0; $i < $iter; $i++ ) {
	foreach my $seq ( $self->each_seq() ) {
	    my $namestr = $self->displayname($seq->get_nse());
	    my $subseq = substr( $seq->seq, $i*$block, $block );
	    print $out sprintf( "%-".$maxn."s  %s\n", $namestr, $subseq );
	}
	if( $self->match_states() ) {
	    my $submatch = substr( $self->match_states(), $i*$block, $block );
	    print $out sprintf( "%-".$maxn."s  %s\n", "\#=GC RF", $submatch );
	}
	if( $ss_str ) {
	    my $subcons  = substr( $ss_str, $i*$block, $block );
	    print $out sprintf( "%-".$maxn."s  %s\n", "\#=GC SS_cons", $subcons );
	}
	print $out "\n" unless( ($i+1) >= $iter );
    }
    print $out "\/\/\n";
}


sub write_connect {
    my $self  = shift;
    my $out   = shift;
    my $seqid = shift; # if this exists we'll use a single sequence and
                       # the SS_cons line

    die "can't read ss_cons line" if( not $self -> ss_cons() );

    my $seqstr;
    if( $seqid ) {
	my( $seq ) = $self->each_seq_with_id($seqid);
	$seqstr = $seq->seq();
    }
    else {
	$seqstr = $self->consensus();
    }

    $seqid = "Seq_cons" unless $seqid;

    my $newaln = Rfam::RfamAlign->new();
    my $newseq = new Bio::LocatableSeq( '-id'  => $seqid,
					'-start' => 1,
					'-end' => $self->length(),
					'-seq' => $seqstr );

    $newaln -> add_seq( $newseq );
    $newaln -> ss_cons( $self->ss_cons );
    $newaln -> allgaps_columns_removed();
    my( $gapless ) = $newaln -> each_seq();
    
    my @ss   = split( //, $newaln->ss_cons->getInfernalString );
    my @seq  = split( //, $gapless->seq() );

    my( @open, %bp );
    for( my $i=1; $i<=@ss; $i++ ) {
	if( $ss[$i-1] =~ /[\[\(\{\<]/ ) {
	    push( @open, $i );
	}
	elsif( $ss[$i-1] =~ /[\]\)\}\>]/ ) {
	    my $j = pop @open || -1;
	    $bp{$i} = $j;
	    $bp{$j} = $i;
	}
    }
    
    # dummy first line
    print $out "  ", $newseq->length, " ENERGY = 0  $seqid\n";

    for( my $i=1; $i<=@ss; $i++ ) {
	my $j;
	if( exists $bp{$i} ) {
	    $j = $bp{$i};
	}
	else {
	    $j = 0;
	}
	my $next;
	if( $i+1 > @ss ) {
	    $next = 0;
	}
	else {
	    $next = $i+1;
	} 
	print $out sprintf( "%5d %-1s %5d %5d %5d %5d\n", $i, $seq[$i-1], $i-1, $next, $j, $i );
    }
}


=head2 write_fasta

 Title     : write_fasta
 Usage     : $ali->write_fasta(\*OUTPUT)
           :
           :
 Function  : writes a fasta formatted alignment
           :
 Returns   :
 Argument  : reference-to-glob to file or filehandle object

=cut

sub write_fasta {
    my $self = shift;
    my $file  = shift;
    my ($seq,$rseq,$name,$count,$length,$seqsub);

    foreach $rseq ( $self->each_seq() ) {
        $name = $self->displayname($rseq->get_nse());
        $seq  = $rseq->seq();
        
        print $file ">$name\n";
        
        $count =0;
        $length = length($seq);
        while( ($count * 60 ) < $length ) {
            $seqsub = substr($seq,$count*60,60);
            print $file "$seqsub\n";
            $count++;
        }
    }
}


# this returns simply the most common base, no fancy perc. cutoffs or anything
sub _compute_consensus {
    my $self = shift;
    my @columns;

    foreach my $seq ( $self -> each_seq() ) {
	my @ary = split( //, $seq->seq );
	for( my $i=0; $i<@ary; $i++ ) {
	    $columns[$i]->{$ary[$i]} ++;
	}
    }

    my $str;
    foreach my $col ( @columns ) {
	my $best;
	my $high = 0;
	foreach my $sym ( keys %{$col} ) {
	    if( $col->{$sym} > $high ) {
		$best = $sym;
		$high = $col->{$sym};
	    }
	}
	$str .= $best;
    }
    return $str;
}


1;
