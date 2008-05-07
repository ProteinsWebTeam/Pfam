#! /software/bin/perl -w

# A program to take Rfam SEEDs (or other alignment input) and build either a quick & dirty tree with quicktree or a
#slow and accurate tree

use strict;
use Rfam;
use Rfam::RfamAlign;
use Getopt::Long;
use DBI;

# MySQL connection details.
my $database = "rfamlive";
my $host     = "pfamdb2a";
my $user     = "pfamadmin";
my $pw       = "mafpAdmin";
my $port     = 3303;

# Query to search for the accession and description of embl entries with the embl id
my $query = qq(
           select t.species, t.tax_string 
           from taxonomy as t, rfamseq as r 
           where t.auto_taxid=r.taxon and rfamseq_acc=?;
   );

#mysql -h pfamdb2a -u pfamadmin -pmafpAdmin -P 3303 rfamlive
#select t.ncbi_id, t.species, t.tax_string from taxonomy as t, rfamseq as r where t.auto_taxid=r.taxon and rfamseq_acc="AE001437";
#select all rfmaseqs with this ncbi_id (nb..NOT a left join)
#select r.rfamseq_acc, r.taxon  from rfamseq as r ,taxonomy as t where r.taxon=t.auto_taxid and t.ncbi_id=272562;

my $infile = "SEED";
my ($quick, $slow, $outlist, $help);
$quick=1;
$outlist=1;
&GetOptions(
            "f|infile=s"       => \$infile,
            "o|outlist"        => \$outlist,
            "q|quickonly"      => \$quick,
            "s|slowalso"       => \$slow,
	    "h|help"           => \$help
    );

if( $help ) {
    &help();
    exit(1);
}

# Create a connection to the database.
my $dbh = DBI->connect(
    "dbi:mysql:$database:$host:$port", $user, $pw, {
	PrintError => 1, #Explicitly turn on DBI warn() and die() error reporting. 
	RaiseError => 1
}    );

# Prepare the query for execution.
my $sth = $dbh->prepare($query);
###########


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
if (defined($outlist)){
    if (-e "out.list"){
	open(OUTLIST, "out.list") or die "Could not open out.list\n[$!]";
    }
    else {
	print "FATAL: could not find the file \42out.list\42\n\n";
	&help();
	exit(1);
    }
    
    while (my $line = <OUTLIST>){
	if ($line =~ m/^(\S+)\t\S+\t(\S+)\t(\d+)\t(\d+)\s+.+/){
	    
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
    if (defined($outlist) && defined($outliststart{$seqobj->id})){
	
	for ( my $i=0; $i< scalar(@{ $outliststart{$seqobj->id} }); $i++ ){
	    	    if (overlap($seqobj->start, $seqobj->end, $outliststart{$seqobj->id}[$i], $outlistend{$seqobj->id}[$i])){
			$score_string = $outlistscore{$seqobj->id}[$i] . "_";
		    }
	}
    }
    elsif (defined($outlist)){
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

system("sreformat phylip $tmpseed > infile")  and die( "FATAL: Error in [sreformat phylip $tmpseed > infile].\n[$!]");

if (-e "outfile"){
    system("rm outfile");
}

if (-e "outtree"){
    system("rm outtree");
}

#Compute a quick and dirty tree
system("echo \42Y\42 | dnadist");
system("mv infile infile.phy");
system("mv outfile infile");
system("echo \42Y\42 | neighbor");

open( TR, "<outtree" ) or die "outtree exists but can't be opened\n[$!]";
my @tree = <TR>;
close(TR);
my $tree = join("", @tree);

foreach my $k (keys %speciesnames){
    $tree =~ s/$k\:/$speciesnames{$k}\:/g;
}

open( TR, ">$infile\.njtree\.dnd" ) or die "FATAL: problem opening $infile\.dnd\n[$!]";
print TR "$tree\n";
close(TR);

if (-e "outtree"){
    system("rm outtree");
}

if (-e "outfile"){
    system("rm outfile");
}

if (defined($quick) && !defined($slow)){
    print "View your tree in $infile\.njtree\.dnd\n";
    exit();
}

#Running dnaml-erate
system("mv infile.phy infile");
system("echo \42Y\42 | dnaml-erate");

open( TR, "<outtree" ) or die "outtree exists but can't be opened\n[$!]";
@tree = <TR>;
close(TR);
$tree = join("", @tree);

foreach my $k (keys %speciesnames){
    $tree =~ s/$k\:/$speciesnames{$k}\:/g;
}

open( TR, ">$infile\.dnaml-erate\.dnd" ) or die "FATAL: problem opening $infile\.dnd\n[$!]";
print TR "$tree\n";
close(TR);

print "View trees in $infile\.dnaml-erate\.dnd and $infile\.njtree\.dnd\n";

#Clean up
if (-e "$tmpseed"){
    system("rm $tmpseed");
}

if (-e "outfile"){
    system("rm outfile");
}

if (-e "outtree"){
    system("rm outtree");
}

exit();

######################################################################

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

seed2tree.pl - build a quick and dirty NJ tree, then tries to build an accurate dnaml-erate tree.
               The NJ trees are written to \47filename\47\.njtree\.dnd, ML trees are written
	       to \47filename\47\.dnaml-erate\.dnd
    
USAGE:   seed2tree.pl <options>

OPTIONS:       
  -h or -help                Show this help.
  -f|-infile <str>           Use \'str\' as input (must be stockholm), default is to use SEED 
                             from the current dir.
  -q|-quickonly              Only generate the quick and dirty NJ-tree.
  -o|-outlist                Read scores in from the out.list file and prepend these to the N/S-E_species strings.

EXAMPLES: 

To produce a quick NJ tree for the ALIGN file with CM scores annotated:
seed2tree.pl -q -o -f ALIGN

REFERENCES:

Rivas and Eddy (2008) Probabilistic Phylogenetic Inference with Insertions and Deletions.

TO ADD:

EOF
}
