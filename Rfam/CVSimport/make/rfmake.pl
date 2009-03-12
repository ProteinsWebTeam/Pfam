#!/software/bin/perl -w

use strict;
use Getopt::Long;
use Bio::SeqFetcher::xdget; #BIOPERL IS EVIL! REPLACE WITH SeqFetch ASAP!
use CMResults;
use Rfam;
use DBI;
use File::Copy;


my( $thr, 
    $inxfile,
    $list,
    $help, 
    $cove,
#    $fasta,
    $trim,
    $overlaps,
    $file,
    $farm,
    @extrafamily,
    @extra_forbidden_terms,
    @taxonomy,
    @forbiddentaxonomy,
    @forbiddentaxonomynot,
    $output);

&GetOptions( "t=s"      => \$thr,
	     "d=s"      => \$inxfile,
	     "l"        => \$list,
	     "overlaps" => \$overlaps,
	     "cove"     => \$cove,
#	     "fa=s"     => \$fasta,
	     "trim=s"   => \$trim,
	     "file=s"   => \$file,
	     "efam|extrafamily=s@"      => \@extrafamily,
	     "efor|extraforbidden=s@"   => \@extra_forbidden_terms,
	     "rt|taxonomy=s@"           => \@taxonomy,
	     "ft|forbiddentaxonomy=s@"  => \@forbiddentaxonomy,
	     "ftn|forbiddentaxonomynot=s@"  => \@forbiddentaxonomynot,
	     "o|output=s"     => \$output,
	     "farm"           => \$farm,
	     "h|help"         => \$help );


if( $help ) {
    &help();
    exit(1);
}

not $inxfile and $inxfile = $Rfam::rfamseq;

if (defined($farm)){
    $inxfile = '/data/blastdb/Rfam/rfamseq/rfamseq.fa';
}

#my $seqinx = Bio::SeqFetcher::xdget->new( '-db' => [$inxfile] );
my $seqinx = Bio::SeqFetcher::xdget->new( '-db' => [$inxfile] );
#$seqinx -> Bio::SeqFetcher::xdget->db($inxfile);

if (!defined($file)){
    $file = "OUTPUT";
}

if (!(-s $file)){
    die "FATAL: $file either doesn't exist or is empty!\n";
}


if (!defined($output)){
    $output = "out.list";
}

my $thrcurr = 5;
my ($local, @family_terms, %family_terms);
if( not $overlaps && -e "DESC" ) {
    open( DESC, "DESC" ) or warn "Can't open DESC to determine global/local requirement\n";
    while( <DESC> ) {
	/^GA\s+(\S+)/ and do {
	    $thrcurr = $1;
	    #$thr = $1 if not defined $thr;
	};
	
	/^ID/ || /^DE/ || /^PI/ || /^TP/ and do {
	    substr($_,0,3) = "";
	    $_ =~ tr/a-z/A-Z/;
	    my @terms = split(/[\_\s+\/\;\(\)\-\,\.\'\"\:]/,$_);
	    push(@family_terms,@terms);
	};
	
	/^BM\s+cmsearch.*-local.*/ and do {
	    $local = 1;
	};
    }
    close DESC;
}

if (!defined($thr)){#If the DESC file is incomplete then we need to set a sensible default:
    $thr = 5;
}

#Fuck - this should work:
#my $fsize = stat($file)->size if -e $file;
my $fsize = `ls -sk $file`;
$fsize =~ /(\d+)\s+\S+/;
$fsize = $1;
if ($fsize > 500000 && !defined($farm) ){#
    reduce_output($file, $thrcurr);
    #"$file\.$$"

    $fsize = `ls -sk $file`;
    $fsize =~ /(\d+)\s+\S+/;
    $fsize = $1;
    
    if ($fsize > 500000 ){#
	die "FATAL: $file is too big ($fsize bytes)! Running rfmake could crash this machine and the other users will hate on you. Hence we're cowardly retreating and dieing. Get Paul some time to write a new and slim rfmake that doesn't crash!\n";	
    }
    print "Reduced $file to $fsize, remove backup file $file\.$$\n";
}


#Add to Rfam.pm:
my %forbidden_family_terms = (
    AND => 1,
    ARCH => 1,
    ARCHAEA => 1,
    ARCHAEAL => 1,
    BACT => 1,
    BACTERIA => 1,
    BACTERIAL => 1,
    BODY => 1,
    CANDIDATE => 1,
    CD => 1,
    CHROMOSOME => 1,
    DATA => 1,
    DNA => 1,
    DOMAIN => 1,
    DS => 1,
    ELEMENT => 1,
    EUK => 1,
    EUKARYOTE => 1,
    EUKARYOTIC => 1,
    EXON => 1,
    FAMILY  => 1,
    FAMILIES => 1,
    FOR => 1,
    FROM => 1,
    GENE => 1,
    GENOME => 1,
    INTERGENIC => 1,
    INTRON => 1,
    NUCLEAR => 1,
    PHAGE => 1,
    PLANT => 1,
    PRIMER => 1,
    PROMOTER => 1,
    PROTEIN => 1,
    REG => 1,
    RNA => 1,
    SEQ  => 1,
    SEQUENCE => 1,
    SMALL   => 1,
    SUBUNIT => 1,
    THE => 1,
    TYPE => 1,
    UTR => 1,
    VIRUS => 1
);

foreach my $t (@family_terms) {
    if ($t =~ /\S+/ && (length($t)>1) && $t =~ /[A-Z]/ && !$forbidden_family_terms{$t} && !$family_terms{$t}){#Family terms should be longer than 1 char, not match any forbidden family terms,  
	$family_terms{$t}=1;
    }
}

@family_terms = keys %family_terms;

if (@extrafamily){ 
    push(@family_terms,@extrafamily);
}

my $family_terms = join(", ",@family_terms);
print STDERR "Using family terms: $family_terms\n";

my @forbidden_terms = qw(
contaminat
pseudogene
pseudo-gene
repeat
repetitive
transpos
);
 
if (@extra_forbidden_terms){
    push(@forbidden_terms, @extra_forbidden_terms);
}

my $forbidden_terms = join(", ",@forbidden_terms);
print STDERR "Using forbidden terms: $forbidden_terms\n";

my (%seedseqs_start,%seedseqs_end, %seedseqs_found);
if( $list ) {
    open( SEED, "SEED" ) or warn "Can't open SEED to determine overlapping hits\n";
    while( <SEED> ) {
	/^(\S+)\/(\d+)\-(\d+)\s+\S+/ and do {
	    my $id = $1;
	    my $a = $2;
	    my $b = $3;
	    my $n = $id . "/" . $a . "-" . $b;
	    $seedseqs_found{$n} = 0;
	    if ($b<$a){
		my $temp=$a;
		$a = $b;
		$b = $temp;
	    }
	    
	    if( int($a) == $a && $a>0 && int($b) == $b && $b>0 ){
		push(@{$seedseqs_start{$1}},$a);
		push(@{$seedseqs_end{$1}},$b);
	    }
	};
	
	/^(\S+)\s+\S+\n/ and do {
	    $seedseqs_found{$1} = 0;
	};
    }
}

my $already;
open( F, $file ) or die;
if( <F> =~ /^\# Rfam/ ) {
    $already = 1;
}
close F;
open( F, $file ) or die;

my $allres = new CMResults;

printf STDERR "Parsing infernal OUTPUT\n";
if( $cove ) {
    $allres -> parse_cove( \*F );
}
else {
    $allres -> parse_infernal( \*F );
}
close F;

if( $trim ) {
    $allres = $allres->filter_on_cutoff( $trim );
}

if( !$already or $trim ) {
    # write a rearranged and slimmed output file
    open( F, ">$file" ) or die("Failed to open $file.\n[$!]");
    $allres -> write_output( \*F );
    close F;
}

my $res = $allres -> remove_overlaps();

if( $list ) {

    my (%desc, %spec);
    $thr = 0 if( not defined $thr );

    my @goodhits = grep{ $_->bits >= $thr } $res->eachHMMUnit();
    my @allnames = map{ $_->seqname } @goodhits;
    
    # MySQL connection details.
    my $database = $Rfam::embl;
    my $host     = "cbi3";
    my $user     = "genero";
    
    # MySQL rfamlive connection details.
    my $rfdatabase = "rfamlive";
    my $rfhost     = "pfamdb2a";
    my $rfuser     = "pfamadmin";
    my $rfpw       = "mafpAdmin";
    my $rfport     = 3303;
    
    # Create a connection to the database.
    my $dbh = DBI->connect(
	"dbi:mysql:$database;$host", $user, "",
	);
    
    my ($rfdbh, $rfsth);
# Create a connection to the database.
    $rfdbh = DBI->connect(
	"dbi:mysql:$rfdatabase:$rfhost:$rfport", $rfuser, $rfpw, {
	    PrintError => 1, #Explicitly turn on DBI warn() and die() error reporting. 
	    RaiseError => 1
	}    );
    
    # Query to search for the accession and description of uniprot entries with the gene name offered.
    my $query = qq(
           select entry.accession_version, description.description
           from entry, description
           where entry.accession_version=?
           and entry.entry_id=description.entry_id;
   );
    
    # Query to search for the accession and description of embl entries with the embl id
    my $rfquery = qq(
           select t.species, t.tax_string, t.ncbi_id 
           from taxonomy as t, rfamseq as r 
           where t.auto_taxid=r.taxon and rfamseq_acc=?;
   );
    
    
# Prepare the query for execution.
    my $sth = $dbh->prepare($query);
# Prepare the query for execution.
    $rfsth = $rfdbh->prepare($rfquery);
    
    foreach my $seqid (@allnames) {
	
	# Run the query 
	$sth->execute($seqid);
	
	my $res = $sth->fetchall_arrayref;
	foreach my $row (@$res){
	    $desc{$row->[0]} .= $row->[1];
	}
	
	$seqid =~ s/(\.\d+)//;
	$rfsth->execute($seqid);
	if (!defined($spec{$seqid})){
	    my $rfres = $rfsth->fetchall_arrayref;
	    my ($species, $taxonomy, $ncbi) = ("", "", "");
	    foreach my $row (@$rfres){
		$species .= $row->[0];
		$taxonomy .= $row->[1];
		$ncbi .= $row->[2];
	    }
	    
	    $spec{$seqid} = $ncbi . "\t" . $species . "\t" . $taxonomy;
	}
	
    }
    $dbh->disconnect;
#    $rfdbh->disconnect;

    #OPEN files for R:
    my %filehandles = (
	seed   => \*OUTSEED,
	align  => \*OUTALIGN,
	family => \*OUTFAM,
	forbid => \*OUTFORBID,
	thresh => \*OUTTHRESH
    );

    my (%counts, @family_scores, @forbidden_scores);
    my $max_score=0;
    
    foreach my $ty (keys %filehandles){
	open( $filehandles{$ty}, ">out.list_$ty\.dat" ) or die("Problem opening out.list_$ty\.dat\n[$!]");
	$counts{$ty}=0;
    }
    
    if ($thrcurr){
	printf OUTTHRESH "$thrcurr\n";
	$counts{'thresh'}++;
    }
    
    if (-e "species"){
	system("cp species species.old");
    }
    
    if (-e $output){
	system("cp $output $output\.old");
    }
    
    open(OUTFILE, ">$output") or die "Could not open $output\n[$!]\n";   
    open(SPECFILE, ">species") or die "Could not open species\n[$!]\n";   
    my $prev_bits = 999999;
    foreach my $unit ( sort { $b->bits <=> $a->bits } $res->eachHMMUnit() ) {
	
	my $term_label;
	if( not exists $desc{$unit->seqname} ) {
	    $desc{$unit->seqname} = "no description available";
	}
	
	if ( ($unit->bits)<$thrcurr && $thrcurr<=$prev_bits ){
	    printf OUTFILE "***********CURRENT THRESHOLD: $thrcurr bits***********\n";
	}
	
	if ($max_score<$unit->bits){
	    $max_score=$unit->bits;
	}
	
	my $seqlabel = "ALIGN";
	if ( defined($seedseqs_start{$unit->seqname}) ){
	    my $id=$unit->seqname;
	    for (my $i=0; $i<scalar(@{$seedseqs_start{$id}}); $i++){
		my $a = $seedseqs_start{$id}[$i];
		my $b = $seedseqs_end{$id}[$i];
		#print "overlap($a,$b,$unit->start_seq, $unit->end_seq)\n";
		if (overlap($a,$b,$unit->start_seq, $unit->end_seq)){
		    $seqlabel = "SEED";
		    printf OUTSEED "%0.2f\n", $unit->bits;
		    $counts{'seed'}++;
		    push(@family_scores,$unit->bits);
		    my $n = $id . "/" . $a . "-" . $b;
		    if ( !defined($seedseqs_found{$n}) ){
			$n = $id . "/" . $b . "-" . $a;
		    }
		    
		    $seedseqs_found{$n}++;
		    last;
		}
	    }
	}
	
	if ($seqlabel =~ /ALIGN/){
	    printf OUTALIGN "%0.2f\n", $unit->bits;
	    $counts{'align'}++;
	}
	
	my $fammatch=0;
	foreach my $ft (@family_terms) {
	    if ($desc{$unit->seqname} =~ m/$ft/i){
		$fammatch=1;
	    }
	}
	
	my $shortid = $unit->seqname;
	$shortid =~ s/(\.\d+)//;
	foreach my $ft (@taxonomy) {
	    if (defined($spec{$shortid}) && $spec{$shortid} =~ m/$ft/i){
		$fammatch=1;
	    }
	}
	
	if ($fammatch){
	    printf OUTFAM "%0.2f\n", $unit->bits;
	    $counts{'family'}++;
	    push(@family_scores,$unit->bits);
	    $term_label .= "T";
	}
	
	my $forbidmatch=0;
	foreach my $ft (@forbidden_terms) {
	    if ($desc{$unit->seqname} =~ m/$ft/i){
		$forbidmatch=1;
	    }
	}
	
	foreach my $ft (@forbiddentaxonomy) {
	    if (defined($spec{$shortid}) && $spec{$shortid} =~ m/$ft/i){
		$forbidmatch=1;
	    }
	}

	foreach my $ft (@forbiddentaxonomynot) {
	    if (defined($spec{$shortid}) && $spec{$shortid} !~ m/$ft/i){
		$forbidmatch=1;
	    }
	}
	

	if ($forbidmatch){
	    printf OUTFORBID "%0.2f\n", $unit->bits;
	    $counts{'forbid'}++;
	    push(@forbidden_scores,$unit->bits);
	    $term_label .= "F";
	}
	
	
	if (!defined($term_label)){
	    $term_label = ".";
	}
	
	
	
	printf OUTFILE "%0.2f\t$seqlabel\t%s\t%d\t%d\t%d\t%d\t$term_label\t%s\n", $unit->bits, $unit->seqname, $unit->start_seq, $unit->end_seq, $unit->start_hmm, $unit->end_hmm, substr($desc{$unit->seqname},0,70);

	my $seqid = $unit->seqname;
	$seqid =~ s/(\.\d+)//;
	if (defined($spec{$seqid})){
	    printf SPECFILE "%0.2f\t$seqlabel\t%s\t%s\n", $unit->bits, $unit->seqname, $spec{$seqid};
	}
	else {
	    printf SPECFILE "%0.2f\t%s\tNo Taxonomy information available\tNA\tNA\n", $unit->bits, $unit->seqname, $spec{$seqid};	    
	}
	
	$prev_bits = $unit->bits;
    }
    
    
    
    #R fails on empty files:
    foreach my $ty (keys %filehandles){
	if ($counts{$ty}==0){
	    my $fh = $filehandles{$ty};
	    printf $fh "0\n";
	    #printf "$filehandles{$ty} $ty counts=$counts{$ty}\n";
	}
    }
    close( OUTSEED);
    close( OUTALIGN);
    close( OUTFAM);
    close( OUTFORBID);
    close( OUTFILE);
    close( SPECFILE);
    
    #Calculate suggested threshold range using family terms as TPs & forbidden terms as FPs:
    @family_scores = sort { $a <=> $b } @family_scores;
    @forbidden_scores = sort { $a <=> $b } @forbidden_scores;
    
    #@family_scores, @forbidden_scores
    #MCC = ($TP*$TN - $FP*$FN)/sqrt(($TP+$FP)*($TP+$FN)*($TN+$FP)*($TN+$FN))
    #ACC = (TP + TN) / (P + N)

    #SEN = TP / (TP + FN)
    #SPC = TN / (FP + TN) = 1 − FPR

    #FDR = FP / (FP + TP)
    #FPR = FP / N = FP / (FP + TN)
    
    open( OUTACC, ">out.list_accuracy\.dat" ) or die("Problem opening out.list_accuracy\.dat\n[$!]");
    print OUTACC "THRESH\tMCC\tACC\tSEN\tSPC\tFDR\tFPR\n";
    my (@MCC, @ACC, @SEN, @SPC, @FDR, @FPR, @THRESH);
    my ($max_MCC, $max_ACC)=(0,0);
    for (my $threshold = 0; $threshold<$max_score; $threshold++){
	my ($TP, $FP, $TN, $FN) = (0, 0, 0, 0);
	foreach my $fam (@family_scores){
	    if ($fam<$threshold){
		$FN++;
	    }
	    else {
		last;
	    }
	}
	$TP = @family_scores - $FN;
	
	foreach my $forbid (@forbidden_scores){
	    if ($forbid<$threshold){
		$TN++;
	    }
	    else {
		last;
	    }
	}
	$FP = @forbidden_scores - $TN;
	
	my $denom = sqrt(($TP+$FP)*($TP+$FN)*($TN+$FP)*($TN+$FN));
	my ($MCC, $ACC, $SEN, $SPC, $FDR, $FPR, $THRESH) = (0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0);
	if ($denom>0){
	    $MCC = ($TP*$TN - $FP*$FN)/$denom;
	}
	
	if (@family_scores + @forbidden_scores>0){
	    $ACC = ($TP + $TN) / (@family_scores + @forbidden_scores);
	}

	if ($TP + $FN>0){
	    $SEN = $TP / ($TP + $FN);
	}

	if ($FP + $TN>0){
	    $SPC = $TN / ($FP + $TN);
	}

	if ($FP + $TP>0){
	    $FDR = $FP / ($FP + $TP);
	}
	
	if ($FP + $TN>0){
	    $FPR = $FP / ($FP + $TN);
	}
	
	if ($max_MCC<$MCC){
	    $max_MCC=$MCC;
	}
	
	if ($max_ACC<$ACC){
	    $max_ACC=$ACC;
	}
	
	push(@MCC,$MCC);
	push(@ACC,$ACC);
	push(@SEN,$SEN);
	push(@SPC,$SPC);
	push(@FDR,$FDR);
	push(@FPR,$FPR);
	push(@THRESH, $threshold);
	print OUTACC "$threshold\t$MCC\t$ACC\t$SEN\t$SPC\t$FDR\t$FPR\n";
    }
    close(OUTACC);
    
    print "Suggested thresholds:
THRESH\tMCC\tACC\tSEN\tSPC\tFDR\tFPR\n";
    for (my $i=0; $i<@MCC; $i++){
	my $printme;
	
	if ($MCC[$i]==$max_MCC){
	    $printme=1;
	}
	
	if ($ACC[$i]==$max_ACC){
	    $printme=1;
	}
	
	if (defined($printme)){
	    printf "%0.3f\t%0.3f\t%0.3f\t%0.3f\t%0.3f\t%0.3f\t%0.3f\n", $THRESH[$i], $MCC[$i], $ACC[$i], $SEN[$i], $SPC[$i], $FDR[$i], $FPR[$i];
	}
	
	if ( $THRESH[$i]<$thrcurr && $thrcurr<=$THRESH[$i+1] ){
	    printf "***********CURRENT THRESHOLD: $thrcurr bits***********\n";
	}

	
    }

    if (-e "warnings"){
	unlink( "warnings" ) or die "can't remove file [warnings]\n";
    }
    
    foreach my $n (keys %seedseqs_found){
	
	if ($seedseqs_found{$n}<1){
	    
	    
	    open( WARN, ">>warnings" ) or warn "Can't open warnings files\n";
	    printf WARN "WARNING: SEED sequence $n was not in the OUTPUT!\n";
	    printf      "WARNING: SEED sequence $n was not in the OUTPUT!\n";
	    close(WARN);
	}
    }
    
    
    #Run R script, making the out.list.pdf figure:
    system("/software/R-2.6.0/bin/R CMD BATCH --no-save /software/rfam/bin/plot_outlist.R") and warn "WARNING: system call for /software/R-2.6.0/bin/R failed. Check binary exists and is executable.\n";
#FOR TESTING:    
#    system("/software/R-2.6.0/bin/R CMD BATCH --no-save ~/scripts/make/plot_outlist.R") and die "system call for /software/R-2.6.0/bin/R failed. Check binary exists and is executable.\n";
    
    if (!(-e "domain_gfx")){
	mkdir "domain_gfx" or die "Can't create directory: domain_gfx\n[$!]";
    }
    
    system("convert -density 600 -geometry 400 out.list.pdf                domain_gfx/out.list.png");
    system("convert -density 600 -geometry 400 out.list.trunc.pdf          domain_gfx/out.list.trunc.png");
    system("convert -density 600 -geometry 400 out.list.accuracy_stats.pdf domain_gfx/out.list.accuracy_stats.png");

    #Cleanup R files:
#    foreach my $ty (keys %filehandles){
#	system( "rm out.list_$ty\.dat" ) and die "File cleanup failed [rm out.list_$ty\.dat]\n"; 
#    }
#    system( "rm out.list_accuracy\.dat" ) and die "File cleanup failed [rm out.list_accuracy\.dat]\n"; 
    
    exit(0);
}
elsif( $overlaps ) {
    my @ols;
    foreach my $seq ( $res->eachHMMSequence() ) {
	foreach my $unit1 ( $seq->eachHMMUnit() ) {
	    foreach my $unit2 ( $seq->eachHMMUnit() ) {
#		    print "$unit1 $unit2\n";
		next if( $unit1->start_seq == $unit2->start_seq and 
			 $unit1->end_seq   == $unit2->end_seq and
			 $unit1->bits      == $unit2->bits );
		if( ( $unit1->start_seq >= $unit2->start_seq and $unit1->start_seq <= $unit2->end_seq ) or
		    ( $unit1->end_seq   >= $unit2->start_seq and $unit1->end_seq   <= $unit2->end_seq ) ) {
		    my( $score ) = sort { $a<=>$b } ( $unit1->bits, $unit2->bits );
		    push( @ols, { 'score' => $score, 'unit1' => $unit1, 'unit2' => $unit2 } );
		}
	    }
	}
    }
    
    foreach my $ol ( sort { $b->{'score'} <=> $a->{'score'} } @ols ) {
	my $unit1 = $ol->{'unit1'};
	my $unit2 = $ol->{'unit2'};
	
	printf( "%-15s%8d%8d%8d%8d%10s\n", $unit1->seqname, $unit1->start_seq, $unit1->end_seq, $unit1->start_hmm, $unit1->end_hmm, $unit1->bits );
	printf( "%-15s%8d%8d%8d%8d%10s\n\n", "", $unit2->start_seq, $unit2->end_seq, $unit2->start_hmm, $unit2->end_hmm, $unit2->bits );
    }
    exit(0);
}

my $atleastonehit;
open( FA, ">$$.fa" ) or die; 
open( SC, ">scores" ) or die;
foreach my $cmseq ( $res->eachHMMSequence() ) {
    foreach my $cmunit ( $cmseq->eachHMMUnit ) {
	next unless $cmunit->bits >= $thr;
	
	my $id    = $cmunit->seqname;
	my $start = $cmunit->start_seq;
	my $end   = $cmunit->end_seq;
	
	my $seq = &get_seq( $id, $start, $end );
	next unless $seq;
	my $seqstr = $seq->seq();
	$seqstr =~ tr/Tt/Uu/;                 # It's RNA dammit! (SRE)
	$seqstr =~ s/(.{1,60})/$1\n/g;
	print FA ">", $seq->id(), "\n$seqstr"; #Parallelise?
	print SC $cmunit->bits, " $id/$start-$end\n";
	$atleastonehit = 1;
    }
}
close FA;
close SC;

if( !$atleastonehit ) {
    warn "no hits\n";
    exit(0);
}

#cmalign options:
my $options = "-o ALIGN";
if( $local ) {
    $options = "-l ".$options." --qdb";
}
else {
    $options .= " --hbanded";
}
printf STDERR "Running: cmalign $options CM $$.fa\n";
system "cmalign $options CM $$.fa" and die "failed to run cmalign";

my $tc_bits = $res -> lowest_true( $thr );
my $nc_bits = $res -> highest_noise( $thr );
$nc_bits = "undefined" if( $nc_bits == -100000 );    # hack!

printf STDERR "Updating DESC file\n";
if( -s "DESC" ) {
    open( DNEW, ">DESC.new" ) or die;
    open( DESC, "DESC" ) or die;
    while(<DESC>) {
	if( /^GA\s+/ ) {
	    printf DNEW ( "GA   %.2f\n", $thr );
	    next;
	}
	if( /^TC\s+/ ) {
	    printf DNEW ( "TC   %.2f\n", $tc_bits );
	    next;
	}
	if( /^NC\s+/ ) {
	    if( $nc_bits eq "undefined" ) {
		printf DNEW ( "NC   %s\n", $nc_bits );		
	    }
	    else {
		printf DNEW ( "NC   %.2f\n", $nc_bits );
	    }
	    next;
	}
	print DNEW $_;
    }
    close DESC;
    close DNEW;

    rename( "DESC", "DESC.old" ) or die;
    rename( "DESC.new", "DESC" ) or die;

}

#######################################



sub get_seq {
    my $id    = shift;
    my $start = shift;
    my $end   = shift;
    my $reverse;

    $reverse = 1 if( $end and $end < $start );
    $seqinx->options( '' );    #reset this

    if( $end ) {
	my $options = "";
        my( $getstart, $getend ) = ( $start, $end );
        if( $reverse ) {
            ( $getstart, $getend ) = ( $end, $start );
	    $options .= "-r ";
        }
	$options .= "-a $getstart -b $getend ";
	$seqinx->options( $options );
    }

    my $seq = new Bio::Seq;
    eval {
        $seq = $seqinx -> get_Seq_by_acc( $id );
    };
    if( $@ or not $seq ) {
        warn "$id not found in your seq db\n";
        return 0;       # failure
    }

    if( $end ) {
	$seq->id( $seq->id."/$start-$end" );
    }

    return $seq;
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
sub reduce_output {
    my ($file, $thr) = @_;    
    
    rename($file, "$file\.$$");    
    open( OUT, "<" . "$file\.$$" ) or die "FATAL: Can't open $file\.$$\n[$!]";
    open( NOUT, ">$file" ) or die "FATAL: Can't open $file\n[$!]";
    
    my $printMe = 0;
    while( <OUT> ) {
	
	if (/^sequence:\s+(\S+)/){
	    $printMe=1;
	}
	elsif (/^hit\s+\d+\s+:\s+(\d+)\s+(\d+)\s+(\S+)\s+bits/){
	    if (defined($thr) && defined($3) && $thr>$3){
		$printMe = 0;
	    }
	    else {
		$printMe = 1;
	    }
	}
	
	print NOUT $_ if $printMe;
	
    }

    close(OUT);
    close(NOUT);
    return 0;
}


######################################################################

sub help {
    print STDERR <<EOF;
    
rfmake.pl - designed to process the results of rfsearch.pl. 
          - rfmake.pl processes OUTPUT files from rfsearch/cmsearch. In the first round \42-l\42
	    is used to produce a \42out.list\42 file that contains a tabular summary of the hits,
	    based upon this output, the score distributions in out.list.pdf \& out.list.trunc.pdf 
	    and the MCC plot in out.list_accuracy.dat the CURATOR chooses a threshold with which 
	    a full ALIGNment is built using the \42-t <bits>\42 option. 

Usage:      rfmake.pl -t <bits> 
            rfmake.pl -l

Options:    MAJOR MODES:
	    -t <bits>
    	    -l                           option lists hits but does not build ALIGN
	    
            I/O
	    -file <infernal output file> Use an alternative cmsearch output [Default: OUTPUT].
	    -d <blastdb>                 Use a different blast database for sesquence fetching.
	    -o|-output <str>             Output file for the \'-l\' option [Default: out.list]
	    
	    THRESHOLDs/PLOTs 
	    -efam|-extrafamily <str>     Add an extra family term for making the histograms. 
	                                 Scans EMBL DE lines for matches to <str>.
	    -efor|-extraforbidden <str>  Add an extra forbidden term for making the histograms. 
	                                 Scans EMBL DE lines for matches to <str>.
	    -rt|-taxonomy <str>          Taxonomy strings matching <str> are treated as true for the
	                                 distribution plots. 
            -ft|-forbiddentaxonomy       Taxonomy strings matching <str> are treated as false for the
	                                 distribution plots.
	    -ftn|-forbiddentaxonomynot   Taxonomy strings NOT matching <str> are treated as false for the
	                                 distribution plots.

	    ARCHAIC UNSUPPORTED OPTIONS:
	    -overlaps                    do something with overlapping hits
	    -cove                        Sean says 'COVE SUX!', so dont be silly, use Infernal.
	    -trim <?>                    dunno, seems to run filter_on_cutoff() function? 
	    
	    -h|-help                     Print this help
	    
To add:
-Taxonomic restrictions for the forbidden/family (FP/TP) terms. Easier when OS and OC have been added to the DESC files. 
-Clean up DBI code - make a function call - also in rfmake.
-add a warning if a seed sequence matches a forbidden term
-Markup up highest scoring representative for each species

-REWRITE:
--don\47t read OUTPUT into memory
--check seqs fetched from DB match the hits...

DO:
init();
    if(\$list){
	make_outlist();
	make_bells() if \$makebells; #R plots etc.
	exit();
    }
    else {
	make_fa_file(); #Use Rob\47s FetchSeqs function. Switch to Sean's sfetch? Fetch on the farm?
	run_mpi_cmalign();
	exit();
    }

EOF
}
