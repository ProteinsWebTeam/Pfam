#!/software/bin/perl -w

#A program for identifying sequences in ALIGN/minidbs to replace SEED seqs that aren't in RFAMSEQ.
#Rough outline:
#1. Read in "warnings" file from rfmake.pl
#2. Dump "ALIGN" seqs to a fasta file (alternatively use pre-specified minidb's)
#3. Search warnings sequences vs ALIGN/minidb sequences with blat 
#4. Find (near-)identical matches to the missing seqs and suggest replacements.
#5. If there are possible replacements for a missing seq in the ALIGN print:
#      1. [num] 
#      2. SEED seqname
#      3. EMBL-ID/NSE of replacement
#      4. Species
#      5. DE
#6. Read in [num] choice or [n]. 
#7. Align foreign-seq and legal-RFAMSEQ-replacement with clustal
#   -check ends are OK - if not try extending...
#8. Slip legal-RFAMSEQ-replacement into SEED(.new) - use Rfam::RfamAlign::merge_alignment?

#Requires:
#sfetch, sreformat (Sean's SQUID package)
#blat, clustalw

use strict;
use Rfam;
use Rfam::RfamAlign;
use Getopt::Long;
use DBI;

my $blatexe = "/software/pfam/bin/blat";

if (!(-x $blatexe)){
    die "$blatexe does not exist and/or is not executable\n";
}

my (@databasefiles,$minidb,$auto,$verbose,$help,@warnings);
my ($mincoverage,$minpid,$maxpid)=(95, 95, 101);

&GetOptions(
    "m|minidb=s"      => \$minidb,
    "mincoverage=s" => \$mincoverage,
    "minpid=s"      => \$minpid,        
    "maxpid=s"      => \$maxpid,        
    "a|auto"        => \$auto,
    "v|verbose"     => \$verbose,
    "h|help"        => \$help
    );

if( $help ) {
    &help();
    exit(1);
}


#READ WARNINGS:
my %warnings;
if (-e "warnings"){
    open( WARN, "<warnings" ) or die ("FATAL: Couldn't open warnings\n [$!]");
    while(my $l = <WARN>){
	if ($l =~ /^WARNING: SEED sequence (.+)\swas not in the OUTPUT\!/){
	    my $n = $1;
	    $warnings{$n}=1;
	}
    }
    close(WARN);
}

my @missingnames = keys %warnings; 

if (scalar(@missingnames) == 0){
    die "warnings file empty! Why the fuck are you running this script?";
}

if (defined($minidb)){
    @databasefiles = glob( "$minidb\.minidb*" );
}
else{
    system("sreformat -d fasta ALIGN > ALIGN.fasta") and die "FAILED: sreformat -d fasta ALIGN > ALIGN.fasta\n[$!]";
    push(@databasefiles, "ALIGN.fasta");
}

if (@databasefiles < 1){
    die "No databases to search!";
}

system("sreformat -d fasta SEED  > SEED.fasta")  and die "FAILED: sreformat -d fasta SEED  > SEED.fasta \n[$!]";
my (%replace, %replace_db); 
foreach my $mn (@missingnames) {
    
    my $repcount = 0;
    my @candidates = ();
    my @candidates_database = ();
    print "#" x 20 . "\t" . $mn . "\t". "#" x 20 . "\n";
    system("sfetch -d SEED.fasta $mn > warnings.fa") and warn "FAILED: sfetch -d SEED.fasta $mn > warnings.fa\n[$!]";    
    my $querylength = 0;
    #Find sequence length:
    open(SEQSTAT, "seqstat -a warnings.fa |");
    while (my $stat = <SEQSTAT>){
	if ($stat =~ /^\*(.+)\s+(\d+)/){
	    $querylength=$2;
	}
    }
    close(SEQSTAT);
    
    foreach my $db (@databasefiles) {
	
	next if !(-e $db);
	
	system("$blatexe -out=blast8 -t=dna -q=dna -minIdentity=95 $db warnings.fa warnings.blat >& /dev/null") 
	    and warn "WARNING:\nblat for $mn failed\n$blatexe -out=blast8 -t=dna -q=dna -minIdentity=95 $db warnings.fa warnings.blat\n";
# Fields [1-12]: 
#Query id, Subject id, % identity, alignment length, mismatches, gap openings, q. start, q. end, s. start, s. end, e-value, bit score
	
	open(BLAT, "< warnings.blat") or die "FATAL: can't open warnings.blat\n[$!]";
	while (my $bl = <BLAT> ){
	    my @bl = split(/\t/,$bl);
	    my $coverage = 100*($bl[7] - $bl[6] + 1)/$querylength;
	    if($minpid<=$bl[2] && $bl[2] <= $maxpid && $mincoverage <= $coverage){
		my @nse = split(/[\/\-]/, $bl[1]);
		if ($nse[2]<$nse[1]){
		    my $tmp = $nse[1];
		    $nse[1] = $nse[2];
		    $nse[2] = $tmp;
		}
		
		my ($desc,$bitscore) = ("",0.0);
		open( OL, "grep $nse[0] out.list | ");
		while (my $oll = <OL>){
		    my @oll = split(/\t/, $oll);
		    my $overlap = overlap($nse[1], $nse[2], $oll[3], $oll[4]);
		    if ($overlap){
			$bitscore = $oll[0];
			$desc = $oll[8];
			chomp($desc);
		    }
		}
		close(OL);
		
		my $species =  "";
		open( SP, "grep $nse[0] species | head -n 1 | ");
		while (my $spl = <SP>){
		    my @species = split(/\t/, $spl);
		    $species = $species[4];
		}
		close(SP);
		
		push(@candidates, $bl[1]);
		push(@candidates_database, $db);
		printf "[$repcount] replace $mn with $bl[1]\? pid=$bl[2] coverage=%0.2f bitscore=$bitscore\t[$species]\t[$desc]\n", $coverage;
		$repcount++;
	    }
	}
	close(BLAT);
    }
    
    if (@candidates){
	$repcount--;
	my $choice = 0;
	if ($repcount>0){
	    print "Select: [0-$repcount or \47n\47]\n";
	}
	else {
	    print "Select: [$repcount or \47n\47]\n";	    
	}
	
	if (!defined($auto)){
	    chomp( $choice = <STDIN> );
	}
	
	if ($choice =~ /n/i) {
	    print "No replacement for $mn\n";
	    push(@warnings,"No replacement for $mn\n");
	}
	elsif (is_integer($choice) && defined($candidates[$choice])){
	    $replace{$mn} = $candidates[$choice];
	    $replace_db{$mn} = $candidates_database[$choice];
	}
	else {
	    print "Stop playing silly buggers you muppet!\n";
	}
    }
    
}

#Read in SEED:
open( SEED, "SEED" ) or die("FATAL: Couldn't open SEED\n [$!]");
my $seed = new Rfam::RfamAlign;
$seed -> read_stockholm( \*SEED );
close(SEED);

my @remove;
foreach my $rep (keys %replace){
    
    if (defined($verbose)){
	print "Replacing $rep with $replace{$rep}\n";
	print "";
    }

    
    system("sfetch -d SEED.fasta $rep                  > warnings.fa")    and warn "FAILED: sfetch -d SEED.fasta $rep                   > warnings.fa\n[$!]";    
    system("sfetch -d $replace_db{$rep} $replace{$rep} >> warnings.fa")   and warn "FAILED: sfetch -d $replace_db{$rep} $replace{$rep} >> warnings.fa\n[$!]";    
    
    open(FA, "< warnings.fa") or die("FATAL: could not open warnings.fa for reading\n[$!]");
    open(FA2, "> warnings.fa2") or die("FATAL: could not open warnings.fa2 for writing\n[$!]");
    my %seqid2short; my $cnt = 0;
    while(my $fa = <FA>){
	#Replace IDs with short unique integers so CLUSTALW doesn't fuck our IDs - also uniqueify given IDs:
	if ($fa =~ /^\>(\S+)/){
	    my $id = $1;
	    my ($id_new, $id_cnt) = ($id, 0);
	    #while (!$seqid2short{$id_new} && $id_cnt < 10){ #Jumping thru hoops to uniqify given IDs:
	#	$id_new = $id . "_" . $id_cnt;
	#	$id_cnt++;
	#    }
	    $id = $id_new;
	    $seqid2short{$cnt} = $id;
	    print FA2 ">" . $cnt . "\n";
	    $cnt++;
	}
	else {
	    print FA2 $fa;
	}
    }
    close(FA);
    close(FA2);
    
    system("clustalw warnings.fa2 >& /dev/null")                           and warn "FAILED: clustalw warnings.fa2 >& /dev/null\n[$!]";
    system("sreformat -r -u a2m warnings.aln > warnings.fa3") and warn "FAILED: sreformat -r -u a2m warnings.aln > warnings.fa3\n[$!]";    
    
    #Add names back:
    open(FA, "< warnings.fa3") or die("FATAL: could not open warnings.fa3 for reading\n[$!]");
    open(FA4, "> warnings.fa4") or die("FATAL: could not open warnings.fa4 for writing\n[$!]");
    while(my $fa = <FA>){
	if ($fa =~ /^\>(\S+)/){
	    my $id = $1;
	    if ($seqid2short{$id}){
		print FA4 ">" . $seqid2short{$id} . "\n";
	    }
	    else {
		print FA4 ">" . $id . "\n";
	    }
	}
	else {
	    print FA4 $fa;
	}
    }
    close(FA);
    close(FA4);
    
    system("sreformat -r -u --pfam stockholm warnings.fa4 > warnings.stk") and warn "FAILED: sreformat -r -u --pfam stockholm warnings.fa4 > warnings.stk\n[$!]";    
    
    #Read in warnings.stk:
    open( WARN, "warnings.stk" ) or die("FATAL: Couldn't open warnings.stk\n [$!]");
    my $warn = new Rfam::RfamAlign;
    $warn -> read_stockholm( \*WARN );
    close(WARN);
    
    my $rep2 = "";
    if ($rep =~ /(\S+)\/\d+\-\d+/){
	$rep2 = $1;
    }
    
    my $badseq;
    foreach my $seq ( $seed->each_seq() ) {
	my $id = $seq->id;
	if ( $id eq $rep || $id eq $rep2 ){#This is evil I know - but read_stockholm munges seq ids in evil ways. 
	    $badseq=$seq;
	    #print "$id eq $rep  || $id eq $rep[0]?\n";
	    last;
	}
    }
    
    
    my ($alnbadseq, $alngoodseq);
    foreach my $seq ( $warn->each_seq() ) {
	my $id = $seq->id;
	if ( $id eq $rep || $id eq $rep2 ){#This is evil I know - but read_stockholm munges seq ids in evil ways. 
	    $alnbadseq=$seq;
	}
	else {
	    $alngoodseq=$seq;
	}
#	print "$id eq $rep  || $id eq $rep2?\n";
    }
    
    if (!defined($alngoodseq)){
	print "Could not map $rep to a replacement sequence (alngoodseq)\n";
	next;
    };

    if (!defined($alnbadseq)){
	print "Could not map $rep to a replacement sequence (alnbadseq)\n";
	next;
    };
    
    #Code nicked from SGJ's fix_aln:
    
    my $new = Bio::LocatableSeq->new();
    $new->id( $alngoodseq->id );
    
    my @s = split( //, $badseq->seq );
    my @ns = split( //, $alngoodseq->seq );
    my @qs = split( //, $alnbadseq->seq );
    
    my $i = 0;  # counter in aln coord
    my $j = 0;  # counter in seq coord
    my $k = -1;  # counter in hsp coord
    
    # infinite loop so need a last
    for( $i=0; $i>-1; $i++ ) { #  ( 0..scalar(@s-1) ) {
	last if( !$s[$i] );
	next if( $s[$i] eq '.' or $s[$i] eq '-' );

	$k++; # do this at the top so next doesn't negate
	$j++;

	# hsp doesn't extend to 5' end
#	if( $alnbadseq->start > $j ) {
#	    $s[$i] = '.';
#	    $k --; # don't increment
#	    next;
#	}
	# hsp doesn't extend to 3' end
	if( $k > length($alnbadseq->seq) ) {
	    $s[$i] = '-';
	    next;
	}

	# match, mismatch or new seq delete
	$s[$i] = $ns[$k];

	# insertion in new seq
	if( $qs[$k] eq '-' ) {
		$seed->insert_column( $i );
		splice( @s, $i, 0, $ns[$k] );
	}
	
    }
    
    my $ts = join( '', @s );
    $ts =~ tr/tT/uU/;
    $new->start( $alngoodseq->start );
    $new->end( $alngoodseq->end );
    $new->seq( $ts );
    $seed->add_seq( $new );
    
    if (defined($verbose)){
    print "
#Replaced
>" . $badseq->id . "\/" . $badseq->start . "\-" . $badseq->end . "\n" .
$badseq->seq  . "\n" .
"#With:
>" . $alngoodseq->id . "\/" . $alngoodseq->start . "\-" . $alngoodseq->end  . "\n" .
$ts . "\n\n";
    }    
    
    if (defined($badseq)){
	push( @remove, $badseq );
    }
    
}

printf "removing %d sequences from SEED\n", scalar(@remove);
foreach my $seq ( @remove ) {
    $seed->remove_seq( $seq );
}

open(SEEDNEW, ">SEED.new") or die "FATAL: can't open SEED.new\n[$!]";
$seed->write_stockholm( \*SEEDNEW );
close(SEEDNEW);

if(@warnings>0){
    print "There were warnings:\n";
    foreach my $w (@warnings){
	print $w;
    }
}


exit(0);

######################################################################
sub overlap {
    my($x1, $y1, $x2, $y2) = @_;
    
    if ( ($x1<=$x2 && $x2<=$y1) || ($x1<=$y2 && $y2<=$y1) || ($x2<=$x1 && $x1<=$y2) || ($x2<=$y1 && $y1<=$y2)  ){
        return 1;
    }
    else {
        return 0;
    }
}

sub is_integer {
    
    my $test = shift;
    
    if (defined($test) && $test =~ /^(\-|\+)?(0|[1-9])\d*$/){
	return 1;
    }
    else {
	return 0;
    }

}




######################################################################
sub help {
    
    print STDERR <<EOF;

rfmake_resolve_warnings.pl - A program for identifying sequences in 
        ALIGN or minidbs to replace SEED seqs that aren\47t in RFAMSEQ.

Rough outline:
1. Read in \42warnings\42 file from rfmake.pl
2. Dump \42ALIGN\42 seqs to a fasta file (alternatively use pre-specified minidb\47s)
3. Search warnings sequences vs ALIGN/minidb sequences with blat 
4. Find (near-)identical matches to the missing seqs and suggest replacements.
5. If there are possible replacements for a missing seq in the ALIGN print:
      1. [num] 
      2. SEED seqname
      3. EMBL-ID/NSE of replacement
      4. Species
      5. DE
6. Read in [num] choice or [n]. 
7. Align foreign-seq and legal-RFAMSEQ-replacement with clustal
   -check ends are OK - if not try extending...
8. Slip legal-RFAMSEQ-replacement into SEED(.new) - use Rfam::RfamAlign::merge_alignment?

Requires:
sfetch, sreformat (Sean\47s SQUID package)
blat, clustalw

Usage:   rfmake_resolve_warnings.pl <options>

Options:       

  -a|-auto                 Just choose the top match without prompting for a number.
  -m|-minidb <num>         Use minidb\47s with id <num> -- warning: this is slow and buggy, 
                           best to use ALIGN instead.
  -mincoverage <num>       Minimal percentage sequence coverage for a sequence to be considered 
                           for replacement (DEFAULT: $mincoverage)
  -minpid                  Minimum percentage identity for a sequence to be considered 
                           for replacement (DEFAULT: $minpid)        
  -maxpid                  Maximum percentage identity for a sequence to be considered 
                           for replacement (DEFAULT: $maxpid)                 
  -v|-verbose              print lots of crap
  -h or -help              show this help

To Add:
-GENERATE UNIQUE SHORT NAMES FOR SEED FIRST...
-ADD WARNINGS WHEN NO REPLACEMENTS FOUND
-FIX UNFORTUNATE GAP COLUMN ADDING BUG - SEE ~pg5/debug/SNOR10004
-FIX UNFORTUNATE NOT REPLACE ANYTHING BUG - SEE ~pg5/debug/EXTND

EOF
}
