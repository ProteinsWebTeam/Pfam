#! /software/bin/perl -w

# A program to take Rfam SEEDs (or other alignment input) and build either a quick & dirty tree with quicktree or a
#slow and accurate tree

use strict;
use Getopt::Long;
use DBI;
use File::Copy;

use Rfam;
use Rfam::RfamAlign;

my $infile = "SEED";
my ($fasttree, $quicktree, $njtree, $mletree, $raxmltree, $verbose, $help);

&GetOptions(
            "f|infile=s"       => \$infile,
            "ft|fasttree"      => \$fasttree,
            "ml|mletree"       => \$mletree,
            "nj|njtree"        => \$njtree,
            "qt|quicktree"     => \$quicktree,
            "rt|raxmltree"     => \$raxmltree,
            "v|verbose"        => \$verbose,
	    "h|help"           => \$help
    );

if( $help ) {
    &help();
    exit(1);
}

#Set default:
$quicktree = 1 if (!defined($fasttree) && !defined($mletree)  && !defined($njtree)  && !defined($quicktree) );

######################################################################
#INITIALIZE FILES BLOCK
# MySQL connection details.
my $database = "rfamlive";
my $host     = "pfamdb2a";
my $user     = "pfamadmin";
my $pw       = "mafpAdmin";
my $port     = 3303;

# Query to search for the accession and description of embl entries with the embl id
#my $query = qq(
#           select t.species, t.tax_string 
#           from taxonomy as t, rfamseq as r 
#           where t.auto_taxid=r.taxon and rfamseq_acc=?;
#   );

# Query to fetch the species name, full taxonomy string and ncbi id for a given embl id:
my $query = qq(
           select t.species, t.tax_string
           from taxonomy as t, rfamseq as r 
           where t.ncbi_id=r.ncbi_id and r.rfamseq_acc=?;
   );

my $dbh = DBI->connect(
    "dbi:mysql:$Rfam::live_rdb_name:$Rfam::rdb_host:$Rfam::rdb_port", $Rfam::rdb_user, $Rfam::rdb_pass, {
	PrintError => 1, #Explicitly turn on DBI warn() and die() error reporting. 
	RaiseError => 1
    }    );


# Prepare the query for execution.
my $sth = $dbh->prepare($query);
######################################################################

#Read stockholm file in
open( SD, "$infile" ) or die ("FATAL: Couldn't open $infile [$!]\n $!\n");
my $seed = new Rfam::RfamAlign;
$seed -> read_stockholm( \*SD );
close(SD);
my @list = $seed->each_seq();
my $self  = new Rfam::RfamAlign;

if (scalar(@list) > 26**4){
    die "FATAL: too many sequences in $infile (must be less than <26^4)";
}


#If required 
my (%outliststart, %outlistend, %outlistscore);
if (-e "out.list" or -e "TABFILE"){
    if (-e "out.list"){
	open(OUTLIST, "out.list") or die "Could not open out.list\n[$!]";
    }
    elsif (-e "TABFILE"){
	system("rfmake.pl -l") and die "Could not find out.list and could not run rfmake.pl\n[$!]";
	open(OUTLIST, "out.list") or die "Could not open out.list\n[$!]";
    }
    else {
	print "FATAL: could not find or generate the file \42out.list\42\n\n";
	&help();
	exit(1);
    }
    
    while (my $line = <OUTLIST>){
## bits  evalue  seqLabel        name         start             end      qStart  qEnd    termlabel       shortSpecies    description
#215.49  5.11e-57        SEED    AM497930.1               1             209      1       211     T       Naegleria       Naegleria sp. NG332 group I like ribozyme GIR1, strain NG332
	if ($line =~ m/^(\S+)\s+\S+\s+\S+\s+(\S+)\s+(\d+)\s+(\d+)\s+/){
	    
	    my ($s, $e);
	    if ($3<$4){
		$s=$3;
		$e=$4;
	    }
	    else {
		$s=$4;
		$e=$3;
	    }
	    
	    push( @{ $outliststart{$2} } ,$s);
	    push( @{ $outlistend{$2} }   ,$e);
	    push( @{ $outlistscore{$2} } ,$1);
	    
	}
    }
    close(OUTLIST);
}

my %speciesnames;
my $count = 0;
foreach my $seqobj ( @list ) {
    
    my $newid = genid($count);
    
    $seqobj->id =~ m/(\S+)\.\d+/;
    my $id = $1;
    
    my $seq = new Bio::LocatableSeq( '-seq'   => $seqobj->seq,
				     '-id'    => $newid,
				     '-start' => $seqobj->start,
				     '-end'   => $seqobj->end, 
				     '-type'  => 'aligned'
	);
    $self -> add_seq($seq);
    $count++;
    
    my ($species, $tax_string); 
    $sth->execute($id);
    my $res = $sth->fetchall_arrayref;
    foreach my $row (@$res){
	$species .= $row->[0];
	$tax_string .= $row->[1];
    }
    
    if( !defined($tax_string) ) {
	$tax_string = "taxonomy unavailable";
    }
    
    if( !defined($species) ) {
	$species = "species unavailable";
    }
    
    my $score_string = "";
    if (defined($outliststart{$seqobj->id})){
	
	for ( my $i=0; $i< scalar(@{ $outliststart{$seqobj->id} }); $i++ ){
	    	    if (overlap($seqobj->start, $seqobj->end, $outliststart{$seqobj->id}[$i], $outlistend{$seqobj->id}[$i])){
			$score_string = $outlistscore{$seqobj->id}[$i] . "_";
		    }
	}
    }
    else{
	$score_string = "NA_";
    }
    
    $species =~ s/\s+/\_/g;
    #print "$id\t$newid\t$species\t$tax_string\n";
    $speciesnames{$newid} = $score_string . $seqobj->id . "/" . $seqobj->start . "-" . $seqobj->end . "_" . $species;
    $speciesnames{$newid} =~ tr/\:\(\)\[\]\,//d; 
}

$dbh->disconnect;

my $ss_cons = $seed->ss_cons->getInfernalString();
my $ss = new Rfam::SS;
$ss -> parseInfernalString( $ss_cons );

$self -> ss_cons( $ss );
my $len = length($list[0]->seq); 
my $tmpseed = "/tmp/$$.$infile";
open(SDOUT, ">$tmpseed" ) or die ("FATAL: Couldn't open $tmpseed\n[$!]");
write_stockholm_lite($self, \*SDOUT, $len);
close(SDOUT);



######################################################################
#RUN METHODS
#Input file in stockholm format is: $tmpseed

unlink("rm outfile") if (-e "outfile");
unlink("outtree") if (-e "outtree");

my %times;

if (defined($njtree)){
#Compute a quick and dirty tree
    unlink("outtree") if (-e "outtree");
    unlink("outfile") if (-e "outfile");
    unlink("infile") if (-e "infile");

    system("sreformat phylip $tmpseed > infile")  and die( "FATAL: Error in [sreformat phylip $tmpseed > infile].\n[$!]");
    
    my $starttime = time();
    system("echo \42Y\42 | dnadist > /dev/null");
    system("mv outfile infile");
    system("echo \42Y\42 | neighbor > /dev/null");
    my $endtime = time();
    $times{'neighbour-joining'} = $endtime - $starttime;
    open( TR,  "<outtree" ) or die "outtree exists but can't be opened\n[$!]";
    open( TRO, ">$infile\.neighbor\.dnd" ) or die "FATAL: problem opening $infile\.dnd\n[$!]";
    fix_node_names(\*TR, \*TRO, \%speciesnames);
    close(TR);
    close(TRO);
}

if (defined($quicktree)){
    unlink("outtree") if (-e "outtree");

    my $starttime = time();
    system("quicktree -boot 100 -in a -out t $tmpseed | tr -d '\n' > outtree") and die "FATAL: cannot run quicktree -in a -out t $tmpseed > outtree\n[$!]";
    my $endtime = time();
    $times{'quicktree'} = $endtime - $starttime;
    
    open( TR,  "<outtree" ) or die "outtree exists but can't be opened\n[$!]";
    open( TRO, ">$infile\.quicktree\.dnd" ) or die "FATAL: problem opening $infile\.dnd\n[$!]";
    fix_node_names(\*TR, \*TRO, \%speciesnames);
    close(TR);
    close(TRO);
        
}

if (defined($fasttree)){
    unlink("outtree") if (-e "outtree");
    unlink("infile") if (-e "infile");

    system("sreformat -d -u a2m $tmpseed > infile")  and die( "FATAL: Error in [sreformat a2m $tmpseed > infile].\n[$!]");
    
    my $starttime = time();
#    system("FastTree -nt -nj -boot 100 < infile > outtree 2> /dev/null") and die "FATAL: cannot run FastTree -nt < infile  > outtree\n[$!]";
    system("FastTree -nt -nj -boot 100 < infile > outtree") and die "FATAL: cannot run FastTree -nt < infile  > outtree\n[$!]";
    my $endtime = time();
    $times{'fasttree'} = $endtime - $starttime;
    
    open( TR,  "<outtree" ) or die "outtree exists but can't be opened\n[$!]";
    open( TRO, ">$infile\.fasttree\.dnd" ) or die "FATAL: problem opening $infile\.dnd\n[$!]";
    fix_node_names(\*TR, \*TRO, \%speciesnames);
    close(TR);
    close(TRO);
}

if($mletree){
#Running dnaml-erate

    unlink("outtree") if (-e "outtree");
    unlink("outfile") if (-e "outfile");
    unlink("infile")  if (-e "infile");
    system("sreformat phylip $tmpseed > infile")  and die( "FATAL: Error in [sreformat phylip $tmpseed > infile].\n[$!]");
    
    my $starttime = time();
    system("echo \42Y\42 | dnaml-erate");
    my $endtime = time();
    $times{'dnaml-erate'} = $endtime - $starttime;
    
    open( TR,  "<outtree" ) or die "outtree exists but can't be opened\n[$!]";
    open( TRO, ">$infile\.dnaml-erate\.dnd" ) or die "FATAL: problem opening $infile\.dnd\n[$!]";
    fix_node_names(\*TR, \*TRO, \%speciesnames);
    close(TR);
    close(TRO);
}

if($raxmltree){
    unlink("outtree") if (-e "outtree");
    unlink("infile") if (-e "infile");
    system("rm -f RAxML_*outtree");
    
    system("sreformat phylip $tmpseed > infile")  and die( "FATAL: Error in [sreformat phylip $tmpseed > infile].\n[$!]");
    my $ss = 0;
    if ($ss_cons =~ /[\<\[\(\{ABCDEFG]/){
	$ss_cons =~ s/[^\(^\)^\<^\>^\[^\]^\{^\}^\.]/\./g;
	open(SS, ">structfile") or die "FATAL: failed to open structfile!\n[$!]";
	print SS "$ss_cons\n";
	close(SS);
	$ss = 1;
    }

#Philip format, 
    my $cmd = "raxmlHPC-SSE3";
    $cmd .= " -m GTRGAMMA";
#              "-m GTRGAMMA"       : GTR + Optimization of substitution rates + GAMMA model of rate
#                                      heterogeneity (alpha parameter will be estimated)
    $cmd .= " -s infile";
    $cmd .= " -S structfile" if (-s "structfile" && $ss );
    $cmd .= " -A S16";
#     -A      Specify one of the secondary structure substitution models implemented in RAxML.
#              The same nomenclature as in the PHASE manual is used, available models:
#              S6A, S6B, S6C, S6D, S6E, S7A, S7B, S7C, S7D, S7E, S7F, S16, S16A, S16B
#              DEFAULT: 16-state GTR model (S16)

    $cmd .= " -n outtree";
#      -n      Specifies the name of the output file.
    $cmd .= " -N 1"; #Number of bootstraps
    
    print  "$cmd > /dev/null" if $verbose;
    my $starttime = time();
    system("$cmd > /dev/null");
    my $endtime = time();
    $times{'raxml'} = $endtime - $starttime;
    
    open( TR,  "<RAxML_bestTree.outtree" ) or die "RAxML_bestTree.outtree can't be opened\n[$!]";
    open( TRO, ">$infile\.raxml\.dnd" ) or die "FATAL: problem opening $infile\.raxml\.dnd\n[$!]";
    fix_node_names(\*TR, \*TRO, \%speciesnames);
    close(TR);
    close(TRO);
    
}

open(TM, ">seed2tree.times");
foreach my $meth ( sort{ $a cmp $b } keys %times) {
    printf TM "%20s\t%20s\n", $meth, $times{$meth};
}
close(TM);
system("cat seed2tree.times");

cleanup();
exit();

######################################################################

sub fix_node_names {
    my $inFh = shift;
    my $outFh = shift;
    my $hash_ref = shift;
    
    my %rename = %$hash_ref;
    
    my @tree = <$inFh>;
    my $tree = join("", @tree);
    
    foreach my $k (keys %speciesnames){
	$tree =~ s/$k\:/$speciesnames{$k}\:/g;
    }
    
    print $outFh "$tree\n";
    return 0;
}

sub cleanup {
    
    #Clean up
    my @cleanup = qw(infile infile.phy outfile outtree);
    push(@cleanup, $tmpseed);
    
    foreach my $f (@cleanup){
	if (-e $f){
	    system("rm $f");
	}
    }
}

sub genid {
    my $place = shift;
    my @alphabet = ('A' .. 'Z');
    my $alphasize = scalar(@alphabet);
    my ($i, $j, $k, $l) = (0, 0, 0, 0);  
    $l = $place % $alphasize;
    $k = int($place/$alphasize) % $alphasize;
    $j = int($place/$alphasize/$alphasize) % $alphasize;
    $i = int($place/$alphasize/$alphasize/$alphasize) % $alphasize;
    my $id = $alphabet[$i] . $alphabet[$j] . $alphabet[$k] . $alphabet[$l];
#    print "$place\t$alphasize\t$id\t$i\t$j\t$k\t$l\n";
    return $id;
}

################

sub write_stockholm_lite {
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
	    my $namestr = $seq->id;
	    my $subseq = substr( $seq->seq, $i*$block, $block );
	    $subseq =~ s/\./\-/g;
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

######################################################################
sub overlap {
    my($x1, $y1, $x2, $y2) = @_;
    
    if($x1 > $y1){
	my $tmp=$x1;
	$x1=$y1;
	$y1=$tmp;
    }
    
    if($x2 > $y2){
	my $tmp=$x2;
	$x2=$y2;
	$y2=$tmp;
    }
    
    if ( ($x1<=$x2 && $x2<=$y1) || ($x1<=$y2 && $y2<=$y1) || ($x2<=$x1 && $x1<=$y2) || ($x2<=$y1 && $y1<=$y2)  ){
        return 1;
    }
    else {
        return 0;
    }
}

######################################################################
sub help {
    
    print STDERR <<EOF;

seed2tree.pl - builds one or more trees for an input alignment.
    
USAGE:   seed2tree.pl <options>

OPTIONS:       
  -h|--help                   Show this help.
  -f|--infile <str>           Use \'str\' as input (must be stockholm), default is to use SEED 
                              from the current dir.
  -ft|--fasttree              Generate a tree using FastTree (Price, Dehal & Arkin (2009))
  -ml|--mletree               Generate a tree using dnaml-erate (Rivas & Eddy (2008))
  -nj|--njtree                Generate a tree using dnadist+neighbour from the Phylip package (Felsenstein (2009))
  -qt|--quicktree             Generate a tree using quicktree (Howe, Bateman, Durbin R (2002)) [default]
  -rt|--raxmltree             Generate a tree using RAxML v 7.2.5 (alpha) 
                              (Stamatakis, et al. http://wwwkramer.in.tum.de/exelixis/software.html)

EXAMPLES: 

To produce a quick NJ tree for the ALIGN file with CM scores annotated:
seed2tree.pl -nj -f ALIGN

REFERENCES:

Rivas and Eddy (2008) Probabilistic Phylogenetic Inference with Insertions and Deletions.

TO ADD:
RaxML has doublet models!

EOF
}
