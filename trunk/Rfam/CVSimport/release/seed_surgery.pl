#!/software/bin/perl -w

# performs seed surgery on Rfam families at rfamseq update time
# (also handy during familiy building!)

use strict;
use Getopt::Long;

use Bio::SeqIO;
#use Bio::Index::Fasta;
use Bio::SearchIO;
#use Bio::Factory::EMBOSS;
use Bio::AlignIO;
use Rfam;
use Rfam::RfamAlign;
use Bio::SeqFetcher::xdget;
use DBI;

my( $noaction,
    $dir,
    $noclean,
    $rename,
    $queue,
    $file,
    $mapfile,
    $verbose,
);

&GetOptions( "noaction" => \$noaction,
	     "dir"      => \$dir,
	     "noclean"  => \$noclean,
	     "rename"   => \$rename,
	     "f=s"      => \$file,
	     "q=s"      => \$queue,
	     "v"        => \$verbose,
	     "map=s"    => \$mapfile,
	     );

if( $dir ) {
    chdir $dir or die "can't chdir to $dir\n";
}

my $inx = Bio::SeqFetcher::xdget->new( '-db' => [$Rfam::rfamseq] );

#Check the sequence accession & version in the RDB- can't read in the embl_sv.txt file anymore
#Rfamlive RDB stuff
my $rdbName=$Rfam::live_rdb_name;
my $rdbHost=$Rfam::rdb_host;
my $rdbUser=$Rfam::rdb_user;
my $rdbPass=$Rfam::rdb_pass;
my $rdbPort=$Rfam::rdb_port;
my $rdbdriver=$Rfam::rdb_driver;

my $rdbAttr = { RaiseError => 1,
                           PrintError => 1 };
my $dsn    = "dbi:$rdbdriver:$rdbName:$rdbHost:$rdbPort";
my $rdb = DBI->connect( $dsn, $rdbUser, $rdbPass, $rdbAttr )
        or die "(EE) ERROR: couldn't connect to database: $!";

# Rfamlive queries
my $asth = $rdb->prepare("select rfamseq_acc, version from rfamseq where rfamseq_acc=?")
  or die '(EE) ERROR: couldn\'t prepare statement to get the : ' . $rdb->errstr;


my %emblsv;
my %cache;
my $db = Rfam::default_db();
my @list;
@list = @ARGV or @list = $db->get_allacc();

foreach my $acc ( @list ) {
    next unless( -d "$acc" );
    warn "Family $acc:\n" if $verbose;
    my( $seq_change, $cosmetic_change );
    my $aln = new Rfam::RfamAlign;
    open( SEED, "$acc/SEED" ) or die;
    $aln -> read_stockholm( \*SEED );
    close SEED;

    my $newaln = Rfam::RfamAlign -> new();
    if( $aln->ss_cons ) {
	$newaln->ss_cons( $aln->ss_cons );
    }
    if( $aln->match_states ) {
	$newaln->match_states( $aln->match_states );
    }

    foreach my $oldseq ( $aln -> each_seq() ) {

	my @tags;
	my( $oldstart, $oldend ) = ( $oldseq->start, $oldseq->end );

	# $seq is the sequence we will add to the new aln object -
	# we can't simply manipulate the existing sequences because
	# everything goes very bad when we change the id of a sequence
	# object that is in an alignment (Que?)
	my $seq = Bio::LocatableSeq -> new( '-id'    => $oldseq->id(),
					    '-start' => $oldseq->start(),
					    '-end'   => $oldseq->end(),
					    '-seq'   => $oldseq->seq(),
					    '-strand' => 1 );
	
	if( $seq->id() =~ /^(\S+)\.(\d+)$/ ) {
	    $seq->accession_number($1);
	    $seq->version($2);
	}
	else {
	    $seq->accession_number( $seq->id() );
	}
	
	if( my $change = &t_to_u( $seq ) ) {
	    push( @tags, "T_TO_U" );
	    $cosmetic_change = 1;
	}
        
      SWITCH: {
	 
	  print STDERR "(ii) Doing query to get accession info from rfamlive\n";
	  my $dbdata;
	  unless( $dbdata= getAcc($seq->accession_number) ) {
	      push( @tags, "DELETE" );
	      undef $seq;
	      $seq_change = 1;
	      print STDERR "(EE) ERROR: no tax data in rfamlive for this $acc!\n";
	      last;
	  }

	  if (! $$dbdata[1] ){die " problem with the version for this",  $seq->accession_number, "\n";}

	  #my ($dbacc, $dbversion) =@$dbdata;
	  #print "'$dbacc' and '$dbversion'\n";
          #print "seem to have data for this $acc?";
	 
	  if( $seq->version() == $$dbdata[1] ){
	      push( @tags, "VERSION_OK" );
#	      last;
	  }
	  else {
	      # change the version and carry on with checks
	      $seq->version( $$dbdata[1] );
	      push( @tags, "VERSION_FIXED" );
	      $cosmetic_change = 1;
	  }

	  my( $newstart, $newend ) = &subseq_is_unchanged( $seq, $$dbdata[1]);
	  if( $newend ) {
	      if( $newstart == $oldstart and $newend == $oldend ) {
		  push( @tags, "SEQ_OK" );
		  last;
	      }

	      $seq->start( $newstart );
	      $seq->end( $newend );
	      push( @tags, "SEQ_FIXED" );
	      $cosmetic_change = 1;
	      last;
	  }

	  # sequence has changed
	  push( @tags, "SEQ_CHANGED" );
	  my( $newseq ) = &align_to_new( $oldseq );
	  if( $newseq ) {
	      $seq = $newseq;
	  }
	  else {
	      push( @tags, "DELETE" );
	      undef $seq;
	  }
	  $seq_change = 1;
      };

	if( $verbose ) {
	    warn $oldseq->id."\t".join( " ", @tags )."\n";
	}

	if( $seq ) {
	    $seq->id( $seq->accession_number.".".$seq->version );
	    $newaln -> add_seq( $seq ) if( $seq );
	}
    }

    if( !$noaction and ( $cosmetic_change or $seq_change ) ) {
	open( NEW, ">$acc/SEEDNEW" ) or die "can't write to $acc/SEEDNEW";
	$newaln -> write_stockholm( \*NEW );
	close NEW;

	rename( "$acc/SEED", "$acc/SEEDOLD" ) or die "can't rename SEED to SEEDOLD";
	rename( "$acc/SEEDNEW", "$acc/SEED" ) or die "can't rename SEEDNEW to SEED";

    }
    if( $seq_change ) {
	warn "Family $acc: The alignment has changed - you must rebuild the family\n\n";
    }
    elsif( $cosmetic_change ) {
	warn "Family $acc: Cosmetic changes - no rebuild needed\n\n";
    }
    else {
	warn "Family $acc: Unchanged - no rebuild needed\n\n";
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
	$cache{$acc} = $inx->get_Seq_by_acc( $acc ) unless exists $cache{$acc};
    };
    if( $@ or not $cache{$acc} ) {
	warn "$acc not found in your database: $@\n";
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
    my $dbversion   = shift;
    my $newseq = &get_seq( $seq->accession_number().".".$dbversion );

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


sub align_to_new {
#    my $oldseq = shift;
#    my $newid = shift || $oldseq->accession_number;
#    my $fullseq = &get_seq( $newid );

#    open( FA, ">$$.fa" ) or die;
#    my $faout = Bio::SeqIO->new( '-fh'     => \*FA,
#				 '-format' => 'Fasta' );
#    $faout -> write_seq( $oldseq );
#    open( FA, ">$$.db" ) or die;
#    $faout -> write_seq( $fullseq );
#    close FA;

#    system "ssearch3 -b 1 -d 1 -E $matevalue -Q -H -n $$.fa $$.db > $$.ssearch" and die;
#    my $in = Bio::SearchIO -> new( -format => 'fasta', 
#				   -file   => "$$.ssearch" ); 
    
#    my $best;
#    while( my $res = $in->next_result ) {
#        while( my $hit = $res->next_hit ) {
#            while( my $hsp = $hit->next_hsp ) {
#		if( !$best or $hsp->score > $best->score ) {
#		    $best = $hsp;
#		}
#	    }
#	}
#    }   

    return undef;
}


##############


sub return_seq {
    my $inx  = shift;
    my $id   = shift;
    my $from = shift;
    my $to   = shift;

    eval {
	$cache{$id} = $inx -> get_Seq_by_acc( $id ) unless exists $cache{$id};
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



sub getAcc {
    my $acc=shift;
    $asth->execute($acc);

    if( $DBI::err ) {
        print STDERR "(WW) WARNING: error executing  query to get accession & version from RDB: "
            . $rdb->errstr . "\n";
        return;
    }

    my @row = $asth->fetchrow();
    if( $asth->err ) {
        print STDERR "(WW) WARNING: error whilst retrieving query asth"
            . $rdb->errstr . "\n";
        return ;
    }
    
    if (@row==0){ return 0;}
    else {return \@row;}

}


