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
use lib "/nfs/team71/pfam/jd7/Rfam/";
use vars qw( $AUTOLOAD @ISA @EXPORT_OK );
use strict;
use IO::File;

use Bio::SimpleAlign;
use Rfam::SS;

#needed to use RDB to get taxonomy for order_by_taxonomy
use Rfam;

@ISA = qw( Bio::SimpleAlign  Rfam );


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


sub merge_alignment {
    # merge alignments based on rf lines
    my $self = shift;

    foreach my $aln ( @_ ) {
	my @newrf = split( //, $self->match_states() );
        my @oldrf = split( //, $aln->match_states() );
	
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

#           print "$i $newrf[$i] $oldrf[$i] $new_lastm $new_nextm $old_lastm $old_nextm ";

            if( $newrf[$i] eq $oldrf[$i] ) {
                undef $insdir;
#               print "\n";
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
                        foreach my $seq ( $aln -> each_seq() ) {
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
#                       print "1=$count1 2=$count2 ";
                    }
                    
                    if( $insdir ) {
                        $inspos = $new_lastm+1;
                    }
                    else {
                        $inspos = $i;
                    }
                    # how many gaps to insert?
                    my @ins;
                    until( $oldrf[$i] ne '.' ) {
                        $i++;
                        push( @ins, '.' );
                    }
                    $i--;

#                   print "INSERT1 $inspos ",scalar(@ins),"\n";
                    splice( @newrf, $inspos, 0, @ins );
                    $self->insert_column( $inspos, join('',@ins) );
                }
                elsif( $newrf[$i] eq '.' ) { 
                    # we need an insert in aln
                    # same decision as above!
                    my $inspos;
                    if( not defined $insdir ) {
                        my( $count1, $count2 ) = (0,0);
                        foreach my $seq ( $self -> each_seq() ) {
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
#                       print "1=$count1 2=$count2 ";
                    }
                    
                    if( $insdir ) {
                        $inspos = $old_lastm+1;
                    }
                    else {
                        $inspos = $i;
                    }

                    my @ins;
                    # how many gaps to insert?
                    until( $newrf[$i] ne '.' ) {
                        $i++;
                        push( @ins, '.' );
                    }
                    $i--;

#                   print "INSERT2 $inspos ",scalar(@ins), "\n";
                    splice( @oldrf, $inspos, 0, @ins );
                    $aln->insert_column( $inspos, join('',@ins) );
                }
                else {
                    # we have a skipped match state
                    # look ahead for the match
                    for( my $j=1; $j<5; $j++ ) {
                        if( $j==4 ) {
                            die "I can't match up your RF lines\n";
                        }
                        if( $newrf[$i+$j] eq $oldrf[$i] ) {
                            splice( @oldrf, $i, 0, '-' );
                            $aln->insert_column( $i, "-" );
                            last;
                        }
                        elsif( $newrf[$i] eq $oldrf[$i+$j] ) {
                            splice( @newrf, $i, 0, '-' );
                            $self->insert_column( $i, "-" );
                            last;
                        }
                    }
                }
            }
            $new_lastm = $i if( $newrf[$i] =~ /\w/ );
            $old_lastm = $i if( $oldrf[$i] =~ /\w/ );
        }
	foreach my $seq ( $aln->each_seq() ) {
	    unless( $self->each_seq_with_id( $seq->id ) ) {
		$self -> add_seq( $seq );
	    }
	}
    }

    return $self;
}

#######
##This subroutine has now been changed to get the taxonomy string from RDB instead of mfetch.
##uses the family acc to get all the taxonomy lines in one query
##

sub order_by_embl_taxonomy {
    my ($self, $acc, $type) = @_;
    #my $acc=shift;
    #my $type=shift; #SEED or ALIGN;
    my $newaln = Rfam::RfamAlign->new();
    my( %tax, %order );
    my @seqs = $self->each_seq;
    
    ## get the tax strings from the rdb
    ##get DB connection parameters from Rfam.pm
  
    my $dbName=$Rfam::live_rdb_name;
    my $dbdriver=$Rfam::rdb_driver;
    my $dbUser=$Rfam::rdb_user;
    my $dbPass=$Rfam::rdb_pass;
    my $dbPort=$Rfam::rdb_port;
    my $dbHost=$Rfam::rdb_host;   
    
    #prepare DB connection and statement handles
    my $dsn    = "dbi:$dbdriver:$dbName:$dbHost:$dbPort";
    my $dbAttr = { RaiseError => 1,  PrintError => 1 };

    # connect
    my $dbh = DBI->connect( $dsn, $dbUser, $dbPass, $dbAttr )
	or die "(EE) ERROR: couldn't connect to database: $!";

    #need different query for SEED or ALIGN file
    my $asth;
    if ($type eq 'SEED'){ 
	$asth=$dbh->prepare( 'select rs.rfamseq_acc, rs.taxonomy from rfamseq as rs join rfam_reg_seed as rrs on rrs.auto_rfamseq=rs.auto_rfamseq join rfam as rf on rrs.auto_rfam=rf.auto_rfam and rf.rfam_acc=?' )
	    or die '(EE) ERROR: couldn\'t prepare query to retrieve the taxonomy info: ' . $dbh->errstr;}
    elsif($type eq 'ALIGN') {
	$asth=$dbh->prepare( 'select rs.rfamseq_acc, rs.taxonomy from rfamseq as rs join rfam_reg_full as rrf on rrf.auto_rfamseq=rs.auto_rfamseq join rfam as rf on rrf.auto_rfam=rf.auto_rfam and rf.rfam_acc=?' )
	    or die '(EE) ERROR: couldn\'t prepare query to retrieve the taxonomy info: ' . $dbh->errstr;}
    else {
	die '(EE) ERROR: dont recognise the file type?'};

    print STDERR "(ii) getting the taxonomy info from rdb for  \"$acc\"\n";
    
    $asth->execute($acc);
    
    if( $DBI::err ) {
	print STDERR "(WW) WARNING: error executing  query to get taxonomy data: "
	    .  $dbh->errstr . "\n";
    }
    
    my $data =  $asth->fetchall_arrayref();
    if( $asth->err ) {
	print STDERR "(WW) WARNING: error whilst retrieving query asth"
	    . $dbh->errstr . "\n";
	return;
    }
    #store all the rdb data for this family SEED or ALIGN file 
    foreach my $row (@$data){
	$tax{$$row[0]}=$$row[1]; #accession and tax string
	print $$row[0], "\t", $$row[1], "\n";
    }
    
    ###-----------------------------------------------------------

    #get array of seq accessions order the seqs by taxonomy
    foreach my $seq ( @seqs ) {
	    my ( $acc ) = $seq->id()=~ /^(\S+?)(\.|$)/;
	    if( !exists $tax{$acc} ) {
		warn "failed to get taxonomy for ", $seq->id, "\n";
		$tax{$acc} = "zz_unknown";
	    }
	    $order{ $seq->id."/".$seq->start."-".$seq->end } = { 'seq' => $seq,
								 'oc'  => $tax{$acc} };
	}

    foreach my $nse ( sort { $order{$a}->{'oc'} cmp $order{$b}->{'oc'} } keys %order ) {
	#print STDERR "$nse\n";
	$newaln->add_seq( $order{$nse}->{'seq'} );
    }

    $newaln->ss_cons( $self->ss_cons );
    $newaln->match_states( $self->match_states );

    return $newaln;
}


# insert a column of gaps after the given column number
sub insert_column {
    my $self = shift;
    my $col  = shift;
    my $char = shift;
    $char = '.' if( not $char );

    if( $col > $self->length() ) {
	die "can't insert_column [$col] greater than aln length [".$self->length()."]\n";
    }

    foreach my $seq ( $self -> each_seq() ) {
	my @seq = split( //, $seq->seq() );
	splice( @seq, $col, 0, $char );
	$seq->seq( join( '', @seq ) );
    }

    if( $self->match_states() ) {
	my @ary = split( //, $self->match_states() );
	splice( @ary, $col, 0, $char );
	$self->match_states( join( '', @ary ) );
    }

    if( my $ss = $self->ss_cons() ) {
	my @ary = split( //, $ss->getInfernalString() );
	splice( @ary, $col, 0, $char );
	my $newss = Rfam::SS -> new();
	$newss->parseInfernalString( join( '', @ary ) );
	$self->ss_cons( $newss );
    }

    return $self;
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


sub write_ilm {
    # write alnment format for ILM input
    my $self = shift;
    my $out  = shift;

    $self -> gap_char('-');
    my $maxn = $self->maxdisplayname_length();
    foreach my $seq ( $self->each_seq() ) {
	my $namestr = $self->displayname($seq->get_nse());
	print $out sprintf( "%-".$maxn."s:%s\n", $namestr, $seq->seq() );
    }
}


sub write_stockholm {
    my $self  = shift;
    my $out   = shift;
    my $block = shift;
    $block = 50 if( not defined $block );
    $block = $self -> length if( not $block );

    my $maxn = $self->maxdisplayname_length() + 2;
    $maxn = 15 if( $maxn < 15 );
    my $iter = $self->length/$block;
    print $out "\# STOCKHOLM 1.0\n\n";

    my $ss_str;
    eval {
	if( $self->ss_cons ) {
	    $self->ss_cons->length( $self->length );
	    $ss_str = $self->ss_cons->getInfernalString();
	}
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

sub write_coloured_html {
    my $self  = shift;
    my $out   = shift;
    my $block = shift;
    $block = 50 if( not defined $block );
    $block = $self -> length if( not $block );

    my $maxn = $self->maxdisplayname_length() + 2;
    $maxn = 15 if( $maxn < 15 );
    my $iter = $self->length/$block;

    open( CSS, ">align.css" ) or die;
    print CSS <<EOF;
#a    { background-color: #ff9999;}
#b    { background-color: #9999ff;}
#c    { background-color: #99ff99;}
#d    { background-color: #FF9900;}
#e    { background-color: #99ffff;}
#f    { background-color: #98C0C0;}
#g    { background-color: #ffff99;}
#h    { background-color: #ff33ff;}
#i    { background-color: #33ffff;}
#j    { background-color: #ffff33;}
#k    { background-color: #2ED70C;}
#l    { background-color: #F4BEF8;}
#m    { background-color: #ff9900;}
#n    { background-color: #B94F32;}
#o    { background-color: #FF0000;}
#p    { background-color: #ffcc99;}
#q    { background-color: #CCCCCC;}
#r    { background-color: #CC3366;}
#s    { background-color: #CCff66;}
#t    { background-color: #Ffcc66;}
#u    { background-color: #7e652f;}

BODY { 
  font-size: 10pt;
  font-family: courier; 
  font-weight: normal;
}

b, .b {
  font-size: 10pt;
  font-family: courier; 
  font-weight: normal;
}

EOF

    print $out <<EOF;
<html>
<head>
<link REL=stylesheet HREF="align.css" TYPE="text/css">
</head>
<body>
<pre>
EOF

    my %colmap = %{ $self->ss_cons->column_colourmap };
    my @alpha = qw( a b c d e f g h i j k l m n o p q r s t u v w x y z );

    my @ss_str;
    eval {
	if( $self->ss_cons ) {
	    $self->ss_cons->length( $self->length );
	    @ss_str = split( //, $self->ss_cons->getInfernalString() );
	}
    };
	
    for( my $i=0; $i < $iter; $i++ ) {
	foreach my $seq ( $self->each_seq() ) {
	    my $namestr = $self->displayname($seq->get_nse());
	    my @seq = split( //, $seq->seq );
	    
	    printf $out sprintf( "%-".$maxn."s  ", $namestr );
	    for( my $j=($i*$block); $j<($i+1)*$block; $j++ ) {
		last if( $j >= @seq ); # last block may be short
		if( my $col = $colmap{$j+1} ) {
		    my $pair = $self->ss_cons->getPairByCol($j+1);
		    my @res = sort ( $seq[$pair->left -1], $seq[$pair->right -1] );
		    if( ( $res[0] eq "C" and $res[1] eq "G" ) or
			( $res[0] eq "A" and $res[1] eq "T" ) or
			( $res[0] eq "A" and $res[1] eq "U" ) or
			( $res[0] eq "G" and $res[1] eq "U" ) or
			( $res[0] eq "G" and $res[1] eq "T" ) ) {

			print $out "<b id=\"".$alpha[$col-1]."\">".$seq[$j]."</b>";
			next;
		    }
		}
		print $out $seq[$j];
	    }
	    print $out "\n";
	}
	if( $self->match_states() ) {
	    my $submatch = substr( $self->match_states(), $i*$block, $block );
	    print $out sprintf( "%-".$maxn."s  %s\n", "\#=GC RF", $submatch );
	}
	if( @ss_str ) {
	    printf $out sprintf( "%-".$maxn."s  ", "#=GC SS_cons" );
	    for( my $j=($i*$block); $j<($i+1)*$block; $j++ ) {
		last if( $j >= @ss_str ); # last block may be short
		if( my $col = $colmap{$j+1} ) {
		    print $out "<b id=\"".$alpha[$col-1]."\">".$ss_str[$j]."</b>";
		}
		else {
		    print $out $ss_str[$j];
		}
	    }
	    print $out "\n";
	}
	print $out "\n" unless( ($i+1) >= $iter );
    }
    print $out "\/\/\n</pre>\n";
}


sub write_coloured_ps {
    # write postscript output with coloured markup
    my $self  = shift;
    my %params = @_;

    my $out       = $params{'-fh'};
    my $fontsize  = $params{'-fontsize'};
    my $lines     = $params{'-lines'};
    my $fitpage   = $params{'-fitpage'};
    my $landscape = $params{'-landscape'};
    my $block     = $params{'-width'};

    $fontsize = 10 if( not $fontsize );
    my $maxn = $self->maxdisplayname_length() + 2;

    if( $landscape ) {
	$block = int( 125/$fontsize * 10 - $maxn ) if( not $block );	    
	$lines = int( 50/$fontsize * 10 ) if( not $lines );
    }
    else {
	$block = int( 82/$fontsize * 10 - $maxn ) if( not $block );
	$lines = int( 72/$fontsize * 10 ) if( not $lines );
    }

    my $offset = $fontsize/4;
    my $iter = $self->length/$block;
    my $numseqs = $self->no_sequences;

    if( $numseqs+2 > $lines ) {
	# can't fit to page
	$fitpage = 0;
    }

    my $orient = "Portrait";
    $orient = "Landscape" if( $landscape );

    my $whoami = `whoami`;
    chomp $whoami;
    my $date = `date`;
    chomp $date;
    my $page = 1;

    print $out <<EOF;
\%!PS-Adobe-3.0
\%\%Title: SEED
\%\%For: $whoami
\%\%Creator: Rfam::RfamAlign
\%\%CreationDate: $date
\%\%DocumentPaperSizes: a4
\%\%Orientation: $orient
\%\%Pages: 1
\%\%EndComments

/dobackground {
  currentpoint
  gsave
    newpath
    moveto
    0 -$offset rmoveto
    dup 0 rlineto
    0 $fontsize rlineto
    neg 0 rlineto
    closepath
    bgcolor aload pop setrgbcolor
    fill
  grestore
} bind def

/dobackgroundstring {
  stringwidth pop
  dobackground
} bind def

/S {
  show
} bind def

/C {
  dup dobackgroundstring
  S
} bind def

/N {
  /y0 y0 $fontsize sub def
  x0 y0 moveto 
} bind def

%%Page: 1 1
EOF

    my( $x0, $y0 ) = ( 40, 780 );
    if( $landscape ) {
	print $out "90 rotate\n";
	( $x0, $y0 ) = ( 40, -40 );
    }

    print <<EOF;
/bgcolor [ 1 1 1 ] def
/Courier-New findfont
$fontsize scalefont
setfont
newpath
/y0 $y0 def
/x0 $x0 def
x0 y0 moveto

(# STOCKHOLM 1.0) S N
N
EOF

    my %colmap = %{ $self->ss_cons->column_colourmap };
    my @colours = ( "1 0.6 0.6",
		    "0.6 0.6 1",
		    "0.6 1 0.6",
		    "0.6 1 0",
		    "0.6 0 1",
		    "0 1 0.6",
		    "1 0 0.6",
		    "0 0.6 1",
		    "1 0.6 0",
		    "0.3 0.3 1",
		    "0.3 1 0.3",
		    "1 0.3 0.3",
		    );
    my @ss_str;
    eval {
	if( $self->ss_cons ) {
	    $self->ss_cons->length( $self->length );
	    @ss_str = split( //, $self->ss_cons->getInfernalString() );
	}
    };
	
    my $l = 0; # number of lines on the page

    for( my $i=0; $i < $iter; $i++ ) {  # for each block
#	print $out "<< I=$i NS=$numseqs L=$lines l=$l P=$page >>\n";
	if( $fitpage and $l+$numseqs+2 > $lines ) {
	    $l = 0;
	    $page ++;
	    print $out <<EOF;
grestore
showpage
\%\%Page: $page $page
/bgcolor [ 1 1 1 ] def
/Courier-New findfont
$fontsize scalefont
setfont
newpath
/y0 780 def
/x0 40 def
x0 y0 moveto
EOF
	}

	foreach my $seq ( $self->each_seq() ) {
	    $l++; # the line count
	    if( !$fitpage and $l > $lines ) {
		$l = 0;
		$page ++;
		print $out <<EOF;
grestore
showpage
\%\%Page: $page $page
EOF
                if( $landscape ) {
		    print $out "90 rotate\n";
		}
		print $out <<EOF;
/bgcolor [ 1 1 1 ] def
/Courier-New findfont
$fontsize scalefont
setfont
newpath
/y0 $y0 def
/x0 $x0 def
x0 y0 moveto
EOF
	    }

	    my $namestr = $self->displayname($seq->get_nse());
	    my @seq = split( //, $seq->seq );
	    
	    printf $out sprintf( "(%-".$maxn."s  ) S ", $namestr );

	    my $lastcol = 0;
	    for( my $j=($i*$block); $j<($i+1)*$block; $j++ ) {
		last if( $j >= @seq ); # last block may be short

		if( my $pair = $self->ss_cons->getPairByCol($j+1) ) {
		    my $col = $colmap{$j+1};
		    my @res = sort ( $seq[$pair->left -1], $seq[$pair->right -1] );
#		    print $out "\n<< $res[0] $res[1] >>\n";
		    if( ( $res[0] =~ /C/i and $res[1] =~ /G/i ) or
			( $res[0] =~ /A/i and $res[1] =~ /T/i ) or
			( $res[0] =~ /A/i and $res[1] =~ /U/i ) or
			( $res[0] =~ /G/i and $res[1] =~ /U/i ) or
			( $res[0] =~ /G/i and $res[1] =~ /T/i ) ) {

			if( $lastcol != $col ) {
			    $lastcol = $col;
			    print $out "\n/bgcolor \[ $colours[$col-1] \] def\n";
			}
			print $out "($seq[$j]) C ";
			next;
		    }
		}
		print $out "($seq[$j]) S ";
	    }
	    print $out "N\n";
	}
	if( @ss_str ) {
	    printf $out sprintf( "(%-".$maxn."s  ) S ", "#=GC SS_cons" );
	    my $lastcol = 0;
	    for( my $j=($i*$block); $j<($i+1)*$block; $j++ ) {
		last if( $j >= @ss_str ); # last block may be short
		if( my $col = $colmap{$j+1} ) {
		    if( $lastcol != $col ) {
			$lastcol = $col;
			print $out "\n/bgcolor \[ $colours[$col-1] \] def\n";
		    }
		    print $out "($ss_str[$j]) C ";
		}
		else {
		    print $out "($ss_str[$j]) S ";
		}
	    }
	    print $out "N\n";
	    $l++; # the line count
	}
	print $out "N\n" unless( ($i+1) >= $iter );
    }
    print $out "(\/\/) S N\n";
    print $out <<EOF;
grestore
showpage

\%\%EOF
EOF

}


sub write_sparse {
    # sparse format (as named by sgj :) is the format where the first
    # sequence is shown in full and then all other sequences are only
    # shown as differences from the first.  "." means we're identical,
    # "-" means a gap
    my $self  = shift;
    my $out   = shift;
    my $refid = shift;
    my $block = shift;
    $block = 50 if( not defined $block );
    $block = $self -> length if( not $block );

    my $maxn = $self->maxdisplayname_length() + 2;
    $maxn = 15 if( $maxn < 15 );
    my $iter = $self->length/$block;
    print $out "\# RFAM SPARSE FORMAT\n\n";

    my $ss_str;
    eval {
	if( $self->ss_cons ) {
	    $self->ss_cons->length( $self->length );
	    $ss_str = $self->ss_cons->getInfernalString();
	}
    };
	
    for( my $i=0; $i < $iter; $i++ ) {
	my $refseq;
	if( $refid ) {
	    ($refseq) = $self->each_seq_with_id( $refid );
	}
	if( not $refseq ) {
	    ($refseq) = $self->each_seq;
	}
	my @refseq = split( //, substr( $refseq->seq, $i*$block, $block ) );

	print $out sprintf( "%-".$maxn."s  %s\n", 
			    $self->displayname($refseq->get_nse()), 
			    join( '', @refseq ) );

	foreach my $seq ( $self->each_seq() ) {
	    # skip if this is our reference sequence
	    next if( $seq->get_nse eq $refseq->get_nse );

	    my $subseq = substr( $seq->seq, $i*$block, $block );
	    my @subseq = split( //, $subseq );
	    my @newseq;
	    for( my $j=0; $j<@refseq; $j++ ) {
		if( $refseq[$j] eq $subseq[$j] ) {
		    push( @newseq, "." );
		}
		elsif( $refseq[$j] =~ /\w/ and $subseq[$j] !~ /\w/ ) {
		    push( @newseq, "-" );
		}
		elsif( $refseq[$j] !~ /\w/ and $subseq[$j] =~ /\w/ ) {
		    push( @newseq, lc($subseq[$j]) );
		}
		else {
		    push( @newseq, uc($subseq[$j]) );
		}
	    }

	    print $out sprintf( "%-".$maxn."s  %s\n", 
				$self->displayname($seq->get_nse()), 
				join( '', @newseq ) );
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
