#! /software/bin/perl -w

# A program to take "good" sequences from the Rfam full ALIGN, adding them to the SEED using the CM, producing a SEED.new file. 

use strict;
use Rfam;
use Rfam::RfamAlign;
use Getopt::Long;
use Log::Log4perl qw(get_logger :levels); #Damn Ben! 
use DBI;
#use Tie::IxHash;  #Use to return hash keys in the order they were added

my $minpid=0.6;
my $maxpid=0.95;
my $nucends=5;
my $bpconsistency=0.75;
my $forbidden=1;
my $info=1;

my @forbidden_terms = qw(
contaminat
pseudogene
pseudo-gene
repeat
repetitive
transpos
);

# MySQL connection details.
my $database = $Rfam::embl;
my $host     = "cbi3";
my $user     = "genero";

# Query to search for the accession and description of embl entries with the embl id
my $query = qq(
           select entry.accession_version, description.description
           from entry, description
           where entry.accession_version=?
           and entry.entry_id=description.entry_id;
   );

# MySQL rfamlive connection details.
my $rfdatabase = "rfamlive";
my $rfhost     = "pfamdb2a";
my $rfuser     = "pfamadmin";
my $rfpw       = "mafpAdmin";
my $rfport     = 3303;

# Query to search for the accession and description of embl entries with the embl id
my $rfquery = qq(
           select t.species, t.tax_string, t.ncbi_id 
           from taxonomy as t, rfamseq as r 
           where t.auto_taxid=r.taxon and rfamseq_acc=?;
   );


#Global variable for the "is_complementary" function - saves reinitialising each time:
my %canonical_basepair = (
    AU => 1,
    AT => 1,
    UA => 1,
    TA => 1,
    CG => 1,
    GC => 1,
    UG => 1,
    GU => 1,
    TG => 1,
    GT => 1,
    RY => 1,
    YR => 1,
    MK => 1,
    KM => 1,
    SS => 1,
    WW => 1
    );

my (@required_terms, @extra_forbidden_terms, @taxonomy, @forbiddentaxonomy, $ont, $scorethreshold, $help);

&GetOptions("min|minpid=s"                 => \$minpid,
            "max|maxpid=s"                 => \$maxpid,
	    "n|nucends=s"                  => \$nucends,
	    "b|bpcons=s"                   => \$bpconsistency,
	    "r|required=s@"                => \@required_terms,
	    "f|forbidden!"                 => \$forbidden,
	    "ef|extraforbidden=s@"         => \@extra_forbidden_terms,
	    "rt|taxonomy=s@"               => \@taxonomy,
	    "ft|forbiddentaxonomy=s@"      => \@forbiddentaxonomy,
	    "ont:i"                        => \$ont,
	    "t|thresh|threshold|s|scorethresh=s"              => \$scorethreshold,
	    "i|info!"                      => \$info,
	    "h|help"                       => \$help
    );

if( $help ) {
    &help();
    exit(1);
}

if (@extra_forbidden_terms){
    push(@forbidden_terms, @extra_forbidden_terms);
}

############Ben's Logging Crap:#############
######
# Get a logger
######
my $logger = get_logger();
$logger->level($DEBUG) if $info;
######
# Explicitly set the logging style and output; this seems damned ridiculous, aren't the defaults sensible?
######
BEGIN {
    # We initialise on the fly
        Log::Log4perl->init( \<<EOF
log4perl.rootLogger=ERROR, SCREEN
log4perl.appender.SCREEN=Log::Log4perl::Appender::Screen
log4perl.appender.SCREEN.mode=append
log4perl.appender.SCREEN.layout=PatternLayout
#log4perl.appender.SCREEN.layout.ConversionPattern=%d %p> %F{1} on %H line %L: %M - %m%n
EOF
        );
}
############Ben's Logging Crap Ends#########

#READ SEED:
open( SEED, "SEED" ) or $logger->logdie("FATAL: Couldn't open SEED\n [$!]");
my $seed = new Rfam::RfamAlign;
$seed -> read_stockholm( \*SEED );
close(SEED);

my (%SEEDhash, %SEEDseqs, %SEEDstarts, %SEEDends); #

#Read SEED into hashes. This maybe daft. Could use "$seed" directly. But then we lose the niceties of hashes. Rewrite "read_stockholm()"?:
foreach my $seqobj ( $seed->each_seq() ) {
    my $longseqname = $seqobj->id . "/" . $seqobj->start . "-" . $seqobj->end;
    $SEEDhash{$longseqname}= 1;
    push( @{ $SEEDstarts{$seqobj->id} }, $seqobj->start );
    push( @{ $SEEDends{$seqobj->id} }, $seqobj->end );
}

#READ ALIGN:
open( ALIGN, "ALIGN" ) or die ("FATAL: Couldn't open ALIGN [$!]\n $!\n");
my $align = new Rfam::RfamAlign;
$align -> read_stockholm( \*ALIGN );
close(ALIGN);
my @alignlist = $align->each_seq();
my $ss_cons = $align->ss_cons->getInfernalString(); #This is damned confusing!

#READ WARNINGS:
my %warnings;
if (-e "warnings"){
    open( WARN, "warnings" ) or die ("FATAL: Couldn't open warnings [$!]\n $!\n");
    while(my $l = <WARN>){
	if (/^WARNING: SEED sequence (.+)\s+/){
	    my $n = $1;
	    $warnings{$n}=1;
	}
	
    }
    
}


#Make a pair table from the secondary structure & initialise basepair hash:
my @table = make_pair_table($ss_cons);
my %basepairs;
my $noseqs = scalar(@alignlist);
my $len = $table[0];
my $nopairs = 0;
for( my $i = 1; $i<$len+1; $i++ ){
    next unless $table[$i];
    
    my $j = $table[$i];
    if ($i<$j){
	my $bpstr = "$i:$j";
	$basepairs{$bpstr} = 0;
	$nopairs++;
    }
}

my $alignlength = length($alignlist[0]->seq);
my (%ALIGNhash, %ALIGNandSEEDhash, %ALIGNnames); #array of hashes to store nuc-counts.

#Read ALIGN info into hashes and test for overlaps with SEED:
foreach my $seqobj ( @alignlist ) {

    my $start = $seqobj->start;
    my $end = $seqobj->end;
    my $longseqname = $seqobj->id . "/" . $start . "-" . $end;

    $ALIGNhash{$longseqname}=uc($seqobj->seq); #A hash of arrays might be more efficient. Saves all the splitting, substr's of length 1. 
    $ALIGNnames{$longseqname}=$seqobj->id;
    
    if (defined($SEEDhash{$longseqname})){
	$ALIGNandSEEDhash{$longseqname}=1;
    }
    elsif ( defined($SEEDstarts{$seqobj->id}) ) {
 	for (my $i=0; $i<scalar(@{ $SEEDends{$seqobj->id} }); $i++){
	    my $e = $SEEDends{$seqobj->id}[$i];
	    my $s = $SEEDstarts{$seqobj->id}[$i];
	    
	    if ($e<$s){
		my $tmp = $s;
		$s = $e;
		$e = $tmp;
	    }
	    
	    if ($end<$start){
		my $tmp = $start;
		$start = $end;
		$end = $tmp;
	    }
	    
	    if (overlap($start, $end, $s, $e)){
		$ALIGNandSEEDhash{$longseqname}=1;
	    }
	}
    }
}

if (defined($ont) && $ont == 0){
    $ont++;
}

#If using ONT option, need to fetch scores:
my %ALIGNscores;
#tie %ALIGNscores, "Tie::IxHash";
if (defined($ont) || defined($scorethreshold)){
    open(SC, "<scores")  or $logger->logdie("FATAL: Couldn't open SEED\n [$!]"); 
    while (my $sc = <SC>){
	if ($sc =~ /(\S+)\s+(\S+)/){
	    if (defined($ALIGNnames{$2})){
		$ALIGNscores{$2}=$1;
	    }
	    else {
		$logger->logwarn("$2 is in scores but does not appear to be in ALIGN!");
	    }
	}
    }
}

my @ALIGNscoreskeys = sort { $ALIGNscores{$b} <=> $ALIGNscores{$a} } keys %ALIGNscores;

# Create a connection to the database.
my $dbh = DBI->connect(
    "dbi:mysql:$database;$host", $user, "", {
	PrintError => 1, #Explicitly turn on DBI warn() and die() error reporting. 
	RaiseError => 1
}    );

# Prepare the query for execution.
my $sth = $dbh->prepare($query);
###########

my ($rfdbh, $rfsth);
# Create a connection to the database.
$rfdbh = DBI->connect(
    "dbi:mysql:$rfdatabase:$rfhost:$rfport", $rfuser, $rfpw, {
	PrintError => 1, #Explicitly turn on DBI warn() and die() error reporting. 
	RaiseError => 1
    }    );

# Prepare the query for execution.
$rfsth = $rfdbh->prepare($rfquery);
###########

my %timer_hash = (
    0 => 1,
    1 => 1,
    2 => 1,
    3 => 1,
    4 => 1,
    5 => 1,
    6 => 1,
    7 => 1,
    8 => 1,
    9 => 1
    );

if (-s 'ALIGN2SEED'){
    system("cp ALIGN2SEED ALIGN2SEED.1");
}

open( OUT, ">ALIGN2SEED" ) or die ("FATAL: Couldn't open ALIGN2SEED [$!]\n $!\n");
my (%minpid, %maxpid, %ALIGN2SEEDcandidates, @names_array, %seen_taxa);
my ($scorerejected, $align2seedcount, $truncrejected, $structrejected, $pidrejected, $descforbidrejected, $descrequirerejected, $taxonomyrejected, $counter) = (0, 0, 0, 0, 0, 0, 0, 0, 0);

#Index taxonomy strings:
if (defined($ont)){
    @names_array = @ALIGNscoreskeys;
    
    foreach my $n (keys %SEEDhash){
	$n =~ m/(\S+)\.\d+\/\d+\-\d+/;
	my $id = $1;
	my $ncbi_id = "";
	$rfsth->execute($id);
	my $rfres = $rfsth->fetchall_arrayref;
	foreach my $row (@$rfres){
	    $ncbi_id  .= $row->[2];
	}
	
	if (defined($seen_taxa{$ncbi_id})){
	    $seen_taxa{$ncbi_id}+=1;
	}
	else {
	    $seen_taxa{$ncbi_id}=1;
	}
    }
    $seen_taxa{0}=$ont;
}
else {
    @names_array = keys %ALIGNnames;
}

#The big loop, locate sequences in align that fulfil our criteria and print to ALIGN2SEED:
BIGLOOP: foreach my $longseqname (@names_array){
    
    if ($info){
	#Progress bar. Some thing to look at for the really long boring runs:
	$counter++;
	my $frac_done = int(10*$counter/$noseqs);
	if ($timer_hash{$frac_done}){
	    my $timer_str=sprintf("%d%% of ALIGN seqs tested\n", 10*$frac_done); 
	    $timer_hash{$frac_done}=0;
	    $logger->info("$timer_str");
	}
    }
    
    #Sequence is not from SEED:
    if (!defined($ALIGNandSEEDhash{$longseqname})){
	$minpid{$longseqname}=1.0;
	$maxpid{$longseqname}=0.0;
	my ($ok,$oks,$oke,$persequence,$pidcounts) = (1,0,0,0.0,0);
	
	
	#Perform the fast and easy checks first, on the first fail goto next seq.
	
	#Check that the sequence ends are OK:
	
	if (defined($scorethreshold)){
	    if ($ALIGNscores{$longseqname} && $ALIGNscores{$longseqname} < $scorethreshold){
		$logger->info("REJECTED: $longseqname score is too low!\t($ALIGNscores{$longseqname})\n");
		$scorerejected++;
		next BIGLOOP;
		
	    }
	    elsif (!$ALIGNscores{$longseqname}) {
		$logger->info("REJECTED: $longseqname score is missing!\t(check \42scores\42 file)\n");
		$scorerejected++;
		next BIGLOOP;
	    }
	}
	
	
	for (my $i=0; $i<$nucends; $i++){
	    my $a= substr($ALIGNhash{$longseqname},$i,1);
	    
	    if (is_nucleotide($a)){
		$oks=1; 
	    }
	    my $b= substr($ALIGNhash{$longseqname},$alignlength-$i-1,1);
	    
	    if (is_nucleotide($b)){
		$oke=1;
	    }
	}
	
	if (!$oks || !$oke){
	    my $a= substr($ALIGNhash{$longseqname},0,$nucends);
	    my $b= substr($ALIGNhash{$longseqname},$alignlength-$nucends,$nucends);
	    $logger->info("REJECTED: $longseqname TRUNCATED!\t(start=$a, end=$b)\n");
	    $truncrejected++;
	    next BIGLOOP;
	}
	
	#Check structure consistency:
	my @seq = split(//,$ALIGNhash{$longseqname});
	if($nopairs>0){
	    foreach my $bpposns ( keys %basepairs ) {
		my @bpposns = split(/:/,$bpposns);
		my $ispair = is_complementary($seq[$bpposns[0]-1],$seq[$bpposns[1]-1]);
		$persequence += $ispair;
	    }
	    $persequence = $persequence/$nopairs;
	}
	else {
	    $persequence = 1.0;
	}
	
	if ($persequence<$bpconsistency){
	    
	    my $structrejected_string = "";
	    if ($info){
		$structrejected_string = sprintf( "REJECTED: $longseqname doesn't match the consensus structure (FCBP=%0.3f<thresh=$bpconsistency)!\n", $persequence); 
                #Logger still doesn't support formatted output! :-(
	    }
	    
	    $logger->info("$structrejected_string");
	    $structrejected++;
	    next BIGLOOP;
	}
	
	my ($species, $tax_string, $ncbi_id); 
	
	$longseqname =~ m/(\S+)\.\d+\/\d+\-\d+/;
	my $id = $1;
	
	$rfsth->execute($id);
	my $rfres = $rfsth->fetchall_arrayref;
	foreach my $row (@$rfres){
	    $species .= $row->[0];
	    $tax_string .= $row->[1];
	    $ncbi_id  .= $row->[2];
	}
	
	if( !defined($tax_string) ) {
	    $tax_string = "taxonomy unavailable";
	}
	    
	if( !defined($species) ) {
	    $species = "species unavailable";
	}

	if( !defined($ncbi_id) ) {
	    $ncbi_id = 0;
	}


	if (defined($ont) && defined($seen_taxa{$ncbi_id}) && $seen_taxa{$ncbi_id} >= $ont){
	    $logger->info("REJECTED: $longseqname taxonomy is already well represented (count=$seen_taxa{$ncbi_id} > thresh=$ont) [$ncbi_id; $tax_string; $species]!\n");
	    $taxonomyrejected++;
	    next BIGLOOP;
	}

	if ( scalar(@taxonomy)>0 ){
	    my $nomatch = 1;
	    foreach my $rft (@taxonomy){
		if ($tax_string =~ m/$rft/i || $species =~ m/$rft/i){
		    $nomatch = 0;
		}
	    }
	    
	    if ($nomatch){
		$logger->info("REJECTED: $longseqname taxonomy did not match your required terms [$ncbi_id; $tax_string; $species]!\n");
		$taxonomyrejected++;
		next BIGLOOP;
	    }
	}
	
	if ( scalar(@forbiddentaxonomy)>0 ){
	    my $nomatch2 = 0;
	    my $matchterm = "";
	    foreach my $rft (@forbiddentaxonomy){
		if ($tax_string =~ m/$rft/i || $species =~ m/$rft/i){
		    $nomatch2 = 1;
		    $matchterm=$rft;
		    last;
		}
	    }
	    
	    if ($nomatch2){
		$logger->info("REJECTED: $longseqname taxonomy did not match your forbidden term: \47$matchterm\47 [$ncbi_id; $tax_string; $species]!\n");
		$taxonomyrejected++;
		next BIGLOOP;
	    }
	}
	
	#Grab seq description, check it passes required & forbidden terms tests: 
	my $desc; 
	$sth->execute($ALIGNnames{$longseqname});
	my $res = $sth->fetchall_arrayref;
	foreach my $row (@$res){
	    $desc .= $row->[1];
	}
	
	if( !defined($desc) ) {
	    $desc = "no description available";
	}
	
	#Check for matches to required desc terms:
	if(@required_terms){
	    my $nomatch = 1;
	    foreach my $rt (@required_terms){
		if ($desc =~ m/$rt/i){
		    $nomatch = 0;
		}
	    }
	    
	    if ($nomatch){
		$logger->info("REJECTED: $longseqname description did not match your required terms [$desc]!\n");
		$descrequirerejected++;
		next BIGLOOP;
	    }
	}
	
	#Check for matches to forbidden desc terms:
	if ($forbidden){
	    foreach my $ft (@forbidden_terms){
		if ($desc =~ m/$ft/i){
		    $logger->info("REJECTED: $longseqname description matched a forbidden term, $ft desc=[$desc]!\n");
		    $descforbidrejected++;
		    next BIGLOOP;
		}
	    }
	}
	
	#Check ALIGN seq is not too similar to the other seqs we've selected:
	foreach my $longseqname3 (keys %ALIGN2SEEDcandidates){
	    my $p2 = pid($ALIGNhash{$longseqname},$ALIGN2SEEDcandidates{$longseqname3});
	    $pidcounts++;
	    if ($p2>=$maxpid){
		$ok = 0;
		$pidrejected++;
		my $pidrejected_str="";
		if ($info){
		     $pidrejected_str = sprintf( "REJECTED: $longseqname too similar to $longseqname3 in ALIGN2SEEDcandidates (id=%0.3f)!\n", $p2); 
		}
		$logger->info("$pidrejected_str");
		
		next BIGLOOP;
	    }
	}
	
	#Calculate min and max PIDs between the ALIGN seq and SEED seqs:
	my $longseqname_max = "";
	foreach my $longseqname2 (keys %ALIGNandSEEDhash){
	    my $p = pid($ALIGNhash{$longseqname},$ALIGNhash{$longseqname2});
	    $pidcounts++;
	    if ($minpid{$longseqname}>$p){
		$minpid{$longseqname}=$p;
	    }

	    if ($maxpid{$longseqname}<$p){
		$maxpid{$longseqname}=$p;
		$longseqname_max = $longseqname2;
	    }
	    
	}
	
	if ($pidcounts==0 || $maxpid{$longseqname}==0.0){
	    $maxpid{$longseqname} = ($minpid+$maxpid)/2;
	}
	
	#If the max PID between ALIGN seq and SEED seqs is outside our limits:
	if ( $maxpid{$longseqname} < $minpid || $maxpid < $maxpid{$longseqname} ){
	    $ok = 0;
	    my $pidrejected_str = "";
	    if ($info){
		$pidrejected_str = sprintf( "REJECTED: $longseqname max pid outside allowed range, ID=$longseqname_max (id=%0.3f)!\n", $maxpid{$longseqname}); 
	    }
	    $logger->info("$pidrejected_str");
	    
	    $pidrejected++;
	    next BIGLOOP;
	}
	
	$align2seedcount++;
	my $ungapped = $ALIGNhash{$longseqname};
	$ungapped =~ s/[,\.\-:_]//g;
	
	if (defined($ont)){
	    if ($seen_taxa{$ncbi_id}){
		$seen_taxa{$ncbi_id}+=1;
	    }
	    else {
		$seen_taxa{$ncbi_id}=1;
	    }
	}

	printf OUT ">$longseqname\t%0.3f\t%0.3f\t$tax_string; $species\t$desc\n$ungapped\n", $maxpid{$longseqname}, $persequence; 
	my $summary_txt = "";
	if ($info){
	    $summary_txt = sprintf( "ACCEPTED: maxpid:%0.3f\tFcbp:%0.3f\t$longseqname\t$desc\n", $maxpid{$longseqname}, $persequence); 
	}
	$logger->info("$summary_txt");
	$ALIGN2SEEDcandidates{$longseqname}=$ALIGNhash{$longseqname};
    }
    else {#Print DE lines for the SEED sequences:
	
	#Grab seq description:
	my $desc; 
	$sth->execute($ALIGNnames{$longseqname});
	my $res = $sth->fetchall_arrayref;
	foreach my $row (@$res){
	    $desc .= $row->[1];
	}

	if( !defined($desc) ) {
	    $desc = "no description available";
	}
	
	my $seedinfo_txt = sprintf( "SEED\t\t$longseqname\t%s\n", substr($desc,0,70));
	$logger->info("$seedinfo_txt");
    }
}
close(OUT);
$dbh->disconnect;

if (scalar(@taxonomy)>0){
    $rfdbh->disconnect;
}

my $tot = $truncrejected+$structrejected+$pidrejected+$descforbidrejected+$descrequirerejected+$taxonomyrejected+$scorerejected;
$logger->info("rejected $tot sequences, TRUNCATED:$truncrejected, STRUCTURE:$structrejected, DESC.forbid:$descforbidrejected, DESC.require:$descrequirerejected, PID:$pidrejected, TAXA.require:$taxonomyrejected, SCORE:$scorerejected");


if ($align2seedcount>0){
    $logger->info( "Building new SEED (adding $align2seedcount squences):");
    
    system("/software/rfam/extras/infernal-0.81/src/cmbuild -F CM.81 SEED") and $logger->logdie("FATAL: Error in: [/software/rfam/extras/infernal-0.81/src/cmbuild -F CM.81 SEED].\n");
    system("/software/rfam/extras/infernal-0.81/src/cmalign --withpknots --withali SEED -o SEED.new CM.81 ALIGN2SEED") and $logger->logdie( "FATAL: Error in [/software/rfam/extras/infernal-0.81/src/cmalign --withpknots --withali SEED -o SEED.new CM.81 ALIGN2SEED].\n");
    
    $logger->info("Added $align2seedcount sequences\n");
    $logger->info("\trejected $tot sequences, TRUNCATED:$truncrejected, STRUCTURE:$structrejected DESC.forbid:$descforbidrejected, DESC.require:$descrequirerejected, PID:$pidrejected, TAXA.require:$taxonomyrejected, SCORE:$scorerejected\n");
}
else {
    $logger->info( "No ALIGN2SEED candidates.\n");
    $logger->info( "rejected $tot sequences, TRUNCATED:$truncrejected, STRUCTURE:$structrejected, DESC.forbid:$descforbidrejected, DESC.require:$descrequirerejected, PID:$pidrejected, TAXA.require:$taxonomyrejected, SCORE:$scorerejected\n");
}

#Finished!
exit();

######################################################################
# Should add these functions to Rfam modules: 
sub overlap {
    my($x1, $y1, $x2, $y2) = @_;
    
    if ( ($x1<=$x2 && $x2<=$y1) || ($x1<=$y2 && $y2<=$y1) || ($x2<=$x1 && $x1<=$y2) || ($x2<=$y1 && $y1<=$y2)  ){
        return 1;
    }
    else {
        return 0;
    }
}

sub pid {
    my $a = shift;
    my $b = shift;
    
    my @a = split(//, $a);
    my @b = split(//, $b);
    my ($sim, $lena, $lenb) = (0, 0, 0);
    
    if (scalar(@a) != scalar(@b)){
	return 0;
    }
    else {
	
 	for (my $i=0; $i<scalar(@b); $i++){
	    if ( (is_nucleotide($a[$i]) || is_nucleotide($b[$i])) && $a[$i] eq $b[$i] ){
		$sim++;
	    }
	    
	    if ( is_nucleotide($a[$i]) ){
		$lena++;
	    }

	    if ( is_nucleotide($b[$i]) ){
		$lenb++;
	    }
	}
    }
    
    my $maxlen = max($lena, $lenb);
    if ($maxlen>0){
	return $sim/$maxlen;
    }
    else {
	return 0;
    }
}


######################################################################
#returns true if input character is a nucleotide (IUPAC codes):
sub is_nucleotide {
    my $a = shift;
    return ($a =~ m/[ACGUTRYWSMKBDHVN]/i) ? 1 : 0; #Tried using "$_", didn't work...
}

######################################################################
#returns true if the two input characters can form a canonical/watson-crick basepair
sub is_complementary {
    my $a = shift;
    my $b = shift;
    my $ab = uc($a . $b);
    
    return (defined($canonical_basepair{$ab} )) ? 1 : 0;
}


######################################################################
#make_pair_table: takes a structure string and returns an array/pair_table.
#                 pair_table[0] contains the length of the string.
#                 pair_table[1..pair_table[0]] contains 0's if no basepair exists 
#                 for this position, otherwise it contains the index for the 
#                 corresponding pair.
#                 Eg. make_pair_table(".((...))") returns:
#                 (8,0,8,7,0,0,0,3,2)
######################################################################
sub make_pair_table {

    my $str = shift;
        
    my (%bpsymbs_posns, @pair_table, $prime5, $prime3, %bpsymbs3p5p, %bpsymbs3p5p_counts);
    my ($count, $unbalanced) = (0,0);

    #Match up the 5' and 3' basepair simbols:
    my %bpsymbs5p3p = (
	'(' => ')',
	'<' => '>',
	'[' => ']',
	'{' => '}',
	A => 'a',
	B => 'b',
	C => 'c',
	D => 'd',
	E => 'e',
	F => 'f',
	G => 'g',
	H => 'h',
	I => 'i'
	);

    my %bpsymbs5p3p_counts = (
	'(' => 0,
	'<' => 0,
	'[' => 0,
	'{' => 0,
	A => 0,
	B => 0,
	C => 0,
	D => 0,
	E => 0,
	F => 0,
	G => 0,
	H => 0,
	I => 0
	);
    
    #We also need the reverse of the above hashes (ie. Match the 3' symbols with the 5'):
    foreach my $symb5p (keys %bpsymbs5p3p){
	my $symb3p = $bpsymbs5p3p{$symb5p};
	$bpsymbs3p5p{$symb3p} = $symb5p;
	$bpsymbs3p5p_counts{$symb3p} = 0;
    }
    
    my %unpairedsymbs = (
	'.' => 1,
	',' => 2,
	'-' => 3,
	':' => 4,
	'_' => 5
	);
    
    my @ss = split(//,$str);
    $pair_table[0]  = length($str);
    
    my $j=0;
    foreach my $char (@ss){
	$j++;
	$pair_table[$j] = 0;
	
	if ( defined( $unpairedsymbs{$char} ) ) {
	    next; #Boring unpaired region.
	}
	elsif ( defined( $bpsymbs5p3p{$char}) ){#Record position of open bps:
	    push( @{ $bpsymbs_posns{$char} }, $j);
	    ++$count;
	    $bpsymbs5p3p_counts{$char}++;
	}
	elsif ( defined( $bpsymbs3p5p{$char}) ){#close bp, save positions of matches in pair_table:
	    my $mchar = $bpsymbs3p5p{$char};
	    $prime5 = pop( @{ $bpsymbs_posns{$mchar} } );
	    $prime3 = $j;
	    $pair_table[$prime3] = $prime5;
	    $pair_table[$prime5] = $prime3;
	    --$count;
	    $bpsymbs3p5p_counts{$char}++;
	}
       	else {
	    printf STDERR "Strange character \"$char\" in secondary structure:\n$str\n";
	}
    }
    
    #Check basepair symbols are all matched:
    foreach my $symb5p (keys %bpsymbs5p3p_counts){
	my $symb3p = $bpsymbs5p3p{$symb5p};
	my $diff = $bpsymbs5p3p_counts{$symb5p} - $bpsymbs3p5p_counts{$symb3p};
	if ($diff!=0){
	    printf STDERR "BAD EVIL AND NASTY: Unbalanced brackets in secondary structure:\n$bpsymbs5p3p_counts{$symb5p}x\'$symb5p\' and $bpsymbs3p5p_counts{$symb3p}x\'$symb3p\'\n";
	    $unbalanced = 1;
	}
    }
    
    
    if ($count != 0 || $unbalanced){
	printf STDERR "Unbalanced basepair symbols in secondary structure:\n$str\n";
	return ();
    }    
    else {
	return @pair_table;
    }
    
}

######################################################################
sub max {
  return $_[0] if @_ == 1;
  $_[0] > $_[1] ? $_[0] : $_[1]
}

sub min {
  return $_[0] if @_ == 1;
  $_[0] < $_[1] ? $_[0] : $_[1]
}

######################################################################
sub help {
    
    my $forbidden_terms = join(',',@forbidden_terms); 
    print STDERR <<EOF;

ALIGN2SEED.pl - reads in Stockholm format files SEED and ALIGN from the current dir. 
                Identifies sequences (S) in the ALIGN file that may be useful for including
                in the SEED. Sequences to be shifted into ALIGN must fulfil some criteria.
                For inclusion Max_ID(S) compared to the SEED seqs in ALIGN must lie between 
                $minpid and $maxpid. Also, to prevent truncated sequences from inclusion into SEED
                we make the restriction that at least one standard nucleotide character must
                lie within the first and last $nucends nucleotides in the ALIGN alignment. The minumum
                basepair consistency is $bpconsistency.
                
Usage:   ALIGN2SEED.pl <options>

Options:       
                           SEQUENCE BASED FILTERS
  -min|-minpid  <num>      Minimum max-pairwise-sequence-identity between S and SEED sequences. Default is $minpid.
  -max|-maxpid  <num>      Maximum max-pairwise-sequence-identity between S and SEED sequences. Default is $maxpid.
  -n|-nucends   <num>      Number of columns from alignment ends within which S must contain a valid nucleotide character. Default is $nucends.
  -b|-bpcons    <num>      Minimum fraction of canonical basepairs (relative to SS_cons) that S must have. Default is $bpconsistency.

                           EMBL-DESCRIPTION BASED FILTERS
  -r|-required  <str>      A required term for DE lines, eg. \"-required tRNA\", only accepts sequences matching this term. 
		           For multiple terms use another instance eg. \"-required transfer\", this matches tRNA \42OR\42 
		           transfer. (Default is the empty string)
  -f|-forbidden            Rejects S\47s with DE lines matching the forbidden terms: $forbidden_terms
  -nof|-noforbidden        Dont use the forbidden terms, default is to use them [forbidden=$forbidden]
  -ef|-extraforbidden      Add additional DE forbidden terms.

                           TAXONOMY BASED FILTERS
  -rt|-taxonomy            Restrict S\47s to tax_strings and species containing a specific string. Eg. \47-t Bacteria -t Archea\47 
                           will only accept bacterial and archeal sequences. 
  -ft|-forbiddentaxonomy   Restrict S\47s to tax_strings and species not containing a specific string. Eg. \47-t sapiens\47 or \47-t mammal\47 
                           will reject human and mammalian sequences respectively. 
  -ont|-ont <int>          Only New Taxa. This option filters sequences from taxa that are already present in the SEED or have been selected 
                           previously. It checks the high scoring sequences first to ensure the highest scoring representative is selected. 
			   Warning: your \42scores\42 file and \42ALIGN\42 should correspond to the same data - check time-stamps. 
			   Optionally add an integer to accept  at most \47int\47 sequences from any taxa. 
			   
			   CM BIT-SCORE BASED FILTERS
  -t|-thresh|-threshold|-s|-scorethresh 
                           <num>    Filters sequences with CM bit-score below num.          
			   Warning: your \42scores\42 file and \42ALIGN\42 should correspond to the same data - check time-stamps. 
                           
                           PRINTING
  -i|-info                 print lots of info
  -noinfo                  dont print lots of info, default is to print [info=$info]
  -h or -help              show this help

To Add:
  -A blacklist of sequences.
  -Add options to map foreign SEEDs:
	-scan warnings file from rfmake, if there are any identical matches to these seqs
	 in the ALIGN then swap them...
EOF
}
