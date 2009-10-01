#!/software/bin/perl -w

use strict;
use Getopt::Long;
use Cwd;
use DBI;
use File::Copy;
use Rfam;
use RfamUtils;
use RfamTimes;
use SeqFetch;

my( $thr, 
    $inxfile,
    $list,
    $help, 
    $file,
    $farm,
    @extrafamily,
    @extra_forbidden_terms,
    @taxonomy,
    @forbiddentaxonomy,
    @forbiddentaxonomynot,
    $output,
    $incldb,
    @warnings,
    $verbose
    );

&GetOptions( "t=s"      => \$thr,
	     "d=s"      => \$inxfile,
	     "l"        => \$list,
	     "file=s"   => \$file,
	     "efam|extrafamily=s@"      => \@extrafamily,
	     "efor|extraforbidden=s@"   => \@extra_forbidden_terms,
	     "rt|taxonomy=s@"           => \@taxonomy,
	     "ft|forbiddentaxonomy=s@"  => \@forbiddentaxonomy,
	     "ftn|forbiddentaxonomynot=s@"  => \@forbiddentaxonomynot,
	     "o|output=s"     => \$output,
	     "farm"           => \$farm,
             "incldb=s"       => \$incldb,
	     "v|verbose"      => \$verbose,
	     "h|help"         => \$help );

if( $help ) {
    &help();
    exit(1);
}

# make sure files are writable by group
umask(002);

#Futsing around trying to get blastdb's sorted out:
my @blastdb;
if (defined($incldb)){
    push(@blastdb,glob( "$Rfam::rfamseq_current_dir/$incldb/*.xnd"));
}

not $inxfile and $inxfile = $Rfam::rfamseq;
#Setting/getting io files:
$output = "out.list" if not defined($output);

if (defined($farm)){
    $inxfile = $Rfam::rfamseq_farm;
}

push(@blastdb,$inxfile);

my (@tmparray, @seqinx);
foreach my $bdb (@blastdb) {
    if ($bdb !~ /rfamseq.fa.xnd/){
	$bdb =~ s/\.xnd$//;
	push(@tmparray, $bdb);
    }
}
@blastdb = @tmparray;


######################################################################
#DESC file:
my $thrcurr = 5;
my $global;
#Validate SEED and DESC files
my $desc;
if (-s 'DESC'){
    $desc = RfamUtils::slurpDesc();
    $thrcurr = $desc->{'GA'} if defined $desc->{'GA'} && RfamUtils::isNumeric($desc->{'GA'});
    
    my @bm = split(/[\;\n]/, $desc->{'BM'});
    foreach my $bm (@bm){
	$bm =~ /cmsearch.*\s-g\s.*/ and do {
	    $global = 1;
	};
    }
}
elsif (not -s 'DESC') {
    $desc = RfamUtils::generateDesc();
}

$list = 1 if not defined $thr;
######################################################################
#Sorting out forbidden terms, family terms, etc.
my (@family_terms, %family_terms, @forbidden_terms);
if ($list){
    foreach my $tag ( qw(ID DE PI TP)  ){
	next if not defined $desc->{$tag};
	my @terms = split(/[\_\s+\/\;\(\)\-\,\.\'\"\:]/,$desc->{$tag});
	push(@family_terms,@terms);
    }
    
    foreach my $t (@family_terms) {
	my $tt=$t;
	$tt=~tr/a-z/A-Z/;
	if ($t =~ /\S+/ && (length($tt)>1) && $tt =~ /[A-Z]/ && not defined $Rfam::forbidden_family_terms{$tt}){#Family terms should be longer than 1 char, not match any forbidden family terms,  
	    $family_terms{$t}=1;
	}
    }
    @family_terms = keys %family_terms;
    
    if (@extrafamily){ 
	push(@family_terms,@extrafamily);
    }
    
    @forbidden_terms = @Rfam::forbidden_terms;
    if (@extra_forbidden_terms){
	push(@forbidden_terms, @extra_forbidden_terms);
    }

#my $family_terms = join(", ",@family_terms);
#print STDERR "Using family terms: $family_terms\n";
#my $forbidden_terms = join(", ",@forbidden_terms);
#print STDERR "Using forbidden terms: $forbidden_terms, Jennifer\n";
}

######################################################################
#Make hashes for checking overlaps with seeds:
my (%seedseqs_start,%seedseqs_end, %seedseqs_strand, %seedseqs_found, %seen_id);
if( $list ) {
    open( SEED, "< SEED" ) or die "Can't open SEED to determine overlapping hits\n";
    while( <SEED> ) {
	/^(\S+)\/(\d+)\-(\d+)\s+\S+/ and do {
	    my ($id,$a,$b,$strand) = ($1,$2,$3,1);
	    my $n = $id . "/" . $a . "-" . $b;
	    next if defined $seen_id{$n};
	    $seen_id{$n}=1;
	    $seedseqs_found{$n}=0;
	    if ($b<$a){
		my $temp=$a;
		$a = $b;
		$b = $temp;
		$strand = -1;
	    }
	    
	    if( int($a) == $a && $a>0 && int($b) == $b && $b>0 ){
		push(@{$seedseqs_start{$1}},$a);
		push(@{$seedseqs_end{$1}},$b);
		push(@{$seedseqs_strand{$1}},$strand);
	    }
	};
	
	/^(\S+)\s+\S+\n/ and do {
	    $seedseqs_found{$1} = 0;
	};
    }
    close(SEED);
}

######################################################################

$file = "TABFILE" if not defined($file);
die "FATAL: $file either doesn't exist or is empty!\n" if not -s $file;

my $prevBits=99999.00;
my $prevExponent=-1;
if( $list ) {
    
    #printf STDERR "Parsing infernal TABFILE -- creating $output and species supplementary files\n";
    
# Create a connection to the database.
    my $rfdbh = DBI->connect(
	"dbi:mysql:$Rfam::live_rdb_name:$Rfam::rdb_host:$Rfam::rdb_port", $Rfam::rdb_user, $Rfam::rdb_pass, {
	    PrintError => 1, #Explicitly turn on DBI warn() and die() error reporting. 
	    RaiseError => 1
	}    );
    
    # Query to search for the description of embl entries with the embl id
    my $queryDesc = qq(
           select description
                   from rfamseq where rfamseq_acc=? and version=?;         
   );
    
    # Query to fetch the species name, full taxonomy string and ncbi id for a given embl id:
    my $queryTax = qq(
           select t.species, t.tax_string, t.ncbi_id 
           from taxonomy as t, rfamseq as r 
           where t.ncbi_id=r.ncbi_id and r.rfamseq_acc=?;
   );
    
    
# Prepare the queries for execution.
    my $sthDesc = $rfdbh->prepare($queryDesc);
    my $sthTax = $rfdbh->prepare($queryTax);
    
    rename($output , $output . '.old') if -e $output;
    rename('species', 'species.old')   if -e 'species';
    open(OUT, "> $output") or die "FATAL: failed to open $output\n[$!]";
    printf OUT "# %0.4s\t%0.12s\t%s\t%0.20s\t%10s\t%10s\tqStart\tqEnd\ttermlabel\t%0.20s\t%0.70s\n", 
    'bits', 'evalue', 'seqLabel', 'name', 'start', 'end', 'shortSpecies', 'description';    
    
    open(SPEC,"> species") or die "FATAL: failed to open species\n[$!]\n";   
    printf SPEC "# %0.4s\t%0.12s\t%0.6s\t%0.20s\t%0.15s\t%0.20s\t%s\n", 
    'bits', 'evalue', 'seqLabel', 'name', 'ncbiId', 'species','taxString';
    
    open(RIN,"> RIN.dat") or die "FATAL: failed to open RIN.dat\n[$!]\n";   
    printf RIN "bits\ttype\ttrueOrFalse\ttax\n";
    open(RINc,"> RIN_counts.dat") or die "FATAL: failed to open RIN_counts.dat\n[$!]\n";   
    printf RINc "cnt\ttax\n";
    my %kingdomCounts;
    
    #If you don't like this you can fuck off!:
    open(F, "grep -v ^'#' $file | sort -k6nr | ") or die "FATAL: could not open pipe for reading $file\n[$!]";
    #Shell grep & sort are a hell of a lot less resource greedy than perl's equivalents:
    
    my %store; #Used to store N/S-E's for overlap checks
    my $printedThresh;
  TABFILE: while (my $tabline = <F>){
      if($tabline=~/(\S+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+\.\d+)\s+(\S+)\s+\d+/){
	  my ($idline,$tStart,$tEnd,$qStart,$qEnd,$bits,$evalue)=($1,$2,$3,$4,$5,$6,$7);
	  my ($name,$start,$end,$strand)=($idline,1,$tEnd-$tStart+1,1);
	  if($idline=~/(\S+)\/(\d+)-(\d+):(1|-1)/){#Support other formats?
	      ($name,$start,$end,$strand)=($1,$2,$3,$4);
	      #print "$name,$start,$end,$strand\t$tStart,$tEnd";
	  }
	  else {
	      next TABFILE;
	  }
	  
	  ($start,$end)=recomputeBoundaries($start,$end,$strand,$tStart,$tEnd);

	  ###################################
	  #Overlap checks. We only want the highest scoring instance
	  #of each overlapping frag.
	  my ($maxOverlapPercent, $overlapName) = overlapCheck($name,$start,$end,\%store);
	  if ($maxOverlapPercent>0.05){#more than 5% overlap, allows for some fuzzy boundaries...
	      print           "overlaps detected for [$name/$start-$end:$strand] with [$overlapName] by [$maxOverlapPercent\%], skipping it.\n" if defined $verbose;
	      push(@warnings, "WARNING: overlaps detected for [$name/$start-$end:$strand] with [$overlapName] by \[$maxOverlapPercent\%\], skipping it.\n") if defined $verbose;
	      next TABFILE;
	  }
	  
	  push( @{ $store{$name} }, { 'start'  => $start,
				      'end'    => $end
		} );
	  ###################################
	  
	  #Fetch info from the DB:
	  my ($description,$species,$shortSpecies,$domainKingdom,$taxString,$ncbiId);
	  if($name=~/(\S+)\.(\d+)/){
	      my ($seqid, $version) = ($1, $2);
	      $sthDesc->execute($seqid, $version);
	      
	      my $res = $sthDesc->fetchall_arrayref;
	      foreach my $row (@$res){
		  $description .= $row->[0];
	      }
	      
	      $sthTax->execute($seqid);
	      my $rfres = $sthTax->fetchall_arrayref;
	      foreach my $row (@$rfres){
		  $species .= $row->[0];
		  $taxString .= $row->[1];
		  $ncbiId .= $row->[2];
	      }
	      $shortSpecies  = species2shortspecies($species);
	      $domainKingdom = tax2kingdom($taxString . '; ' . $species . ';');
	      $kingdomCounts{$domainKingdom}=0 if not defined $kingdomCounts{$domainKingdom};
	      $kingdomCounts{$domainKingdom}++;
	  }
	  
	  $description  = 'NA' if not defined $description;
	  $species      = 'NA' if not defined $species;
	  $shortSpecies = 'NA' if not defined $shortSpecies;
	  
	  my $seqLabel = 'ALIGN';
	  my $seqLabelExtent = '';
	  my $overlapExtent = overlapsSeed($name,$start,$end,\%seedseqs_start, \%seedseqs_end, \%seedseqs_strand, \%seedseqs_found);
	  if ($overlapExtent>0.1){#Must overlap significantly to be considered:
	      $seqLabel = 'SEED';
	      $seqLabelExtent = sprintf ".%d", int(100*$overlapExtent);
	  }
	  $seqLabel    = 'NOT' if (($bits<$thrcurr) && ($seqLabel ne 'SEED'));
	  my $termlabel;
	  $termlabel .= 'T' if stringMatches($description,\@family_terms);
	  $termlabel .= 'F' if stringMatches($description,\@forbidden_terms);
	  $termlabel  = '.' if not defined $termlabel;
	  
	  if ( $bits < $thrcurr && $thrcurr<=$prevBits){
	      printf OUT  "#***********CURRENT THRESHOLD: $thrcurr bits***********#\n";
	      printf SPEC "#***********CURRENT THRESHOLD: $thrcurr bits***********#\n";
	      printf RIN  "%0.2f\tTHRESH\t\.\t.\n", $thrcurr;
	      $printedThresh=1;
	  }
	  $prevBits = $bits;
	  
	  my $exponent = 1;
	  $exponent = -1 if $evalue =~ /\d+e\-\d+/;
	  if ($exponent > 0 && $prevExponent < 0){
	      printf OUT  "#***********THE EXPONENT HAS CHANGED SIGN***********#\n";
	      printf SPEC "#***********THE EXPONENT HAS CHANGED SIGN***********#\n";
	  }
	  $prevExponent=$exponent;
	  
	  printf OUT  "%0.2f\t%0.12s\t%0.6s\t%0.20s\t%10d\t%10d\t$qStart\t$qEnd\t$termlabel\t%0.20s\t%0.70s\n", $bits, $evalue, $seqLabel, $name, $start, $end, $shortSpecies, $description;
	  printf SPEC "%0.2f\t%0.12s\t%0.6s\t%0.20s\t%15d\t%0.20s\t%s\n", $bits, $evalue, $seqLabel, $name, $ncbiId, $species,$taxString;
	  printf RIN  "%0.2f\t%0.6s\t$termlabel\t$domainKingdom\n", $bits, $seqLabel;
	  
      }#closes regex if
  }#closes TABFILE while
    $rfdbh->disconnect;
    
    if ( not defined $printedThresh){
	printf OUT  "#***********CURRENT THRESHOLD: $thrcurr bits***********#\n";
	printf SPEC "#***********CURRENT THRESHOLD: $thrcurr bits***********#\n";
	printf RIN  "%0.2f\tTHRESH\t\.\t\.\n", $thrcurr;
    }
    
    foreach my $king ( sort{ $a cmp $b } keys %kingdomCounts) {
	printf RINc  "%d\t$king\n", $kingdomCounts{$king};
    }
    
    close(F);
    close(OUT);
    close(SPEC);
    close(RIN);
    close(RINc);
    #Hard-coded paths are naughty!!! Here are 2:
    system("/software/R-2.6.0/bin/R CMD BATCH --no-save ~pg5/scripts/make/plot_outlist.R") and warn "WARNING: system call for /software/R-2.6.0/bin/R failed. Check binary exists and is executable.\n[/software/R-2.6.0/bin/R CMD BATCH --no-save ~pg5/scripts/make/plot_outlist.R]\n";
    
    #Complain loudly if seed sequences are missing from the output:
    foreach my $n (keys %seedseqs_found){
	if ($seedseqs_found{$n}<1){
	    printf          "WARNING: SEED sequence $n was not in the OUTPUT!\n";
	    push(@warnings, "WARNING: SEED sequence $n was not in the OUTPUT!\n");
	}
    }
        
    open( WARN, ">warnings" ) or warn "Can't open warnings files\n";
    foreach my $w (@warnings){
	print WARN $w;
    }
    close(WARN);
    
    
    exit(0);
}

######################################################################
die "FATAL: threshold is undefined!" if not defined $thr;

my ($forward,$reverse)=outlist2fetchSeqsHashes($output,$thr);

open( FA, ">$$.fa" ) or die "FATAL: failed to open $$.fa\n[$!]"; 
foreach my $blastdatabase (@blastdb){
    SeqFetch::fetchSeqs($forward, $blastdatabase, 0, \*FA, 0, 1);
    SeqFetch::fetchSeqs($reverse, $blastdatabase, 1, \*FA, 0, 1);
}
close(FA);

#cmalign options:
my $options = "-o ALIGN";
if( not defined $global ) {
    $options = " -l ".$options;
}

#Complain if cmalign bits scores are negative:
open (CMAO, "> cmalign.out") or die "FATAL: failed to open cmalign.out\n[$!]";
open (CMA, "/usr/bin/time -f \'\%S \%U\' -o cmalign.time cmalign $options CM $$.fa | ") or die "FATAL: failed to run [cmalign $options CM $$.fa]\n[$!]";
while (my $cma = <CMA>){
    if($cma =~ /\s+\d+\s+\S+\s+\d+\s+(-\d+\.\d+)/){
	#Print warning if cmalign bit score is significantly different from the cmsearch bit score?
	print          "WARNING: negative cmalign bits score: $cma";
	push(@warnings,"WARNING: negative cmalign bits score: $cma");    
}
    print CMAO $cma;
}
close(CMA);
close(CMAO);

######################################################################
#Record cpu time:
my $pwd = getcwd;
my %runTimes;
if (defined $desc->{'AC'} && length($desc->{'AC'})>0){
    $runTimes{'rfam_acc'}=$desc->{'AC'};
}
elsif (defined $desc->{'ID'} && length($desc->{'ID'})>0){
    $runTimes{'rfam_acc'}=$desc->{'ID'};    
}
else {
    my @pwd = split(/\//,$pwd);
    $runTimes{'rfam_acc'} = '';
    while(length($runTimes{'rfam_acc'})==0 && @pwd){
	$runTimes{'rfam_acc'} = pop(@pwd);
    }
}

$runTimes{'cmalign'}=0;
open(T, "< cmalign.time") or die "FATAL: failed to open cmalign.time\n[$!]";
while (<T>){
    if(/(\d+\.\d+)\s+(\d+\.\d+)/){
	$runTimes{'cmalign'}+=($1+$2);
    }
}
close(T);

my $schema = RfamTimes->connect("dbi:mysql:host=$Rfam::rdb_host;port=$Rfam::rdb_port;dbname=rfam_times",$Rfam::rdb_user,$Rfam::rdb_pass);
my $rfamTimes   = $schema->resultset('RfamTimes')
    ->update_or_create(%runTimes
		       ,);

######################################################################
#update DESC file! 
my ($tc_bits, $nc_bits) = findTcNc( $thr, $output );

$desc->{'GA'}=$thr;
$desc->{'TC'}=$tc_bits;
$desc->{'NC'}=$nc_bits;

#RfamUtils::writeDesc($desc);
copy('DESC', $$ . '.DESC') if -e 'DESC';
open(DE, "> DESC") or die "FATAL: failed to open DESC\n[$!]";
RfamUtils::writeDesc($desc,\*DE);
close(DE);

#Commented out, screws up a useful check in rfmake_resolve_warnings:
# if(@warnings){
# open( WARN, ">> warnings" ) or warn "Can't open warnings files\n";
# foreach my $w (@warnings){
#     printf WARN $w;
# }
# close(WARN);
# }
exit(0);

######################################################################
#lowest_true:
sub findTcNc {
    my ($thr, $output) = @_;
    my $nc_bits=0;
    my $tc_bits=99999;
    open(OP, "< $output") or die "FATAL: failed to open $output\n[$!]";
    #print "opened $output\n";
    OUT: while(my $op=<OP>){
	if(defined $op && $op=~/(\d+\.\d+)\s+\S+\s+\S+\s+\S+\s+\d+\s+\d+/){
	    my $bits = $1;
	    $tc_bits = $1 if (scalar($thr) <= scalar($bits) && scalar($bits)<scalar($tc_bits));
#	    print "$output: $bits\tthr=$thr > $bits TC $tc_bits\n";
	    if (scalar($thr) > scalar($bits) && scalar($bits)>scalar($nc_bits)){
		$nc_bits = $1;
		#last OUT;
#		print "NC   $nc_bits\n";
	    }
	}
    }
    
    $nc_bits = "undefined" if not defined $nc_bits or $nc_bits==0;
#    print "($tc_bits, $nc_bits)\n";
    return ($tc_bits, $nc_bits);
}

######################################################################
#recomputeBoundaries: given padded sequence coordinates and the cmsearch target coordinates, recompute the sequence start-end.
sub recomputeBoundaries {
    my ($start,$end,$strand,$tStart,$tEnd)=@_;
    my ($s,$e);
    if ($strand>0){
	$s = $start+$tStart-1;
	$e = $start+$tEnd-1;
    }
    else {
	$e = $end-$tEnd+1;
	$s = $end-$tStart+1;
    }
    
    return ($s,$e);
}
######################################################################
#overlapCheck: return true if the N/S-E overlaps with something in the %store hash:
sub overlapCheck{
    my ($name,$start,$end,$store)=@_;
    return (0,'') if not defined $store->{$name};
    
    ($start,$end) = RfamUtils::reorder($start,$end);
    my ($maxOverlap,$overlapNSE)=(0,'');
    for (my $i=0; $i<scalar(@{$store->{$name}}); $i++){
	my $a = $store->{$name}[$i]{'start'};
	my $b = $store->{$name}[$i]{'end'};
	($a,$b) = RfamUtils::reorder($a,$b);
	if (RfamUtils::overlap($a,$b,$start,$end)){
	    $maxOverlap = RfamUtils::max($maxOverlap, RfamUtils::overlapExtent($a,$b,$start,$end));
	    $overlapNSE = $name . '/' . $store->{$name}[$i]{'start'} . '-' . $store->{$name}[$i]{'end'};
	    #return RfamUtils::overlapExtent($a,$b,$start,$end);
	}
    }
    return ($maxOverlap, $overlapNSE);
}

######################################################################
#overlapsSeed: check if the given N/S-E overlaps with a seed sequence: 
sub overlapsSeed {
    my ($name,$start,$end,$seedseqs_start, $seedseqs_end, $seedseqs_strand, $seedseqs_found)=@_;
    return 0 if not defined $seedseqs_start->{$name};
    
    ($start,$end) = RfamUtils::reorder($start,$end);
#[0] [7] replace M10217.1/5909-5841 with M10217.1/5910-5840? [pid=100.00 cover=100.00] [blat=135.0 bits=54.91 E=3.67e-07] [.] [X.laevis] [EMBL;STD;VRT:Xenopus laevis mitochondrial DNA, complete genome.]
# 1:M10217.1/5910-5840
# 2:M10217.1/5910-5840
# 3:M10217.1/5840-5910    2136-2204
# 3:M10217.1/5840-5910    4724-4798
# 3:M10217.1/5840-5910    5770-5840
# 4:M10217.1/5840-5910    5770-5840 OVERLAPS!
# WARNING: SEED sequence M10217.1/5909-5841 was not in the OUTPUT!
# WARNING: SEED sequence M10217.1/5910-5978 was not in the OUTPUT!   
# Use overlapExtent to make sure overlap is > 0.9...
    my $maxOverlapExtent=0;
    for (my $i=0; $i<scalar(@{$seedseqs_start->{$name}}); $i++){
	my $a = $seedseqs_start->{$name}[$i];
	my $b = $seedseqs_end->{$name}[$i];
	($a,$b) = RfamUtils::reorder($a,$b);
	
	if (RfamUtils::overlap($a,$b,$start,$end)){
	    my $n = "$name/$a-$b";
	    $n = "$name/$b-$a" if $seedseqs_strand->{$name}[$i] < 0;
	    $seedseqs_found->{$n}++;
	    #return RfamUtils::overlapExtent($a,$b,$start,$end);
	    $maxOverlapExtent = RfamUtils::max($maxOverlapExtent, RfamUtils::overlapExtent($a,$b,$start,$end));
	}
    }
    return $maxOverlapExtent;
}

######################################################################
#descriptionMatches: 
sub stringMatches {
    my ($description, $terms) = @_;
    
    foreach my $t (@{$terms}){
	return 1 if $description =~ /$t/;
    }
    return 0;
}

######################################################################
#outlist2fetchSeqsHashes: create %forward & %reverse hashes of arrays of hashes for sequence fetching
sub outlist2fetchSeqsHashes {
    my ($outlist,$threshold) = @_;
    open(OL, "< $outlist") or die "FATAL: failed to open $outlist\n[$!]";
    open( SC, ">scores" )  or die "FATAL: failed to open scores\n[$!]";
    my (%forward, %reverse);
    while (<OL>){
	if(/(\S+)\s+\S+\s+(SEED\.\d+|SEED|ALIGN|NOT)\s+(\S+)\s+(\d+)\s+(\d+)/){
	    my ($bits,$id,$start,$end)=($1,$3,$4,$5);
	    last if $bits < $threshold;
	    
	    if ($start < $end){
		push( @{ $forward{$id} }, { 'start'  => $start,
					    'end'    => $end,
					    'strand' => 1} );
	    }
	    elsif ($start > $end) {
		push( @{ $reverse{$id} }, { 'start'  => $end,
					    'end'    => $start,
					    'strand' => -1} );
	    }
	    print SC $bits, " $id/$start-$end\n";
	}
    }
    close(OL);
    close(SC);
    
    return (\%forward, \%reverse);
}

######################################################################
#
sub species2shortspecies {
    my $species = shift;
    my $shortSpecies;
    
    if ($species=~/(\S+)\s+sp\./){
	$shortSpecies = $1;
    }
    elsif ($species=~/metagenome/i){
	$species=~s/metagenome/metag\./g;
	my @w = split(/\s+/,$species);
	if(scalar(@w)>2){
	    foreach my $w (@w){
		$shortSpecies .= substr($w, 0, 5) . '.';
	    }
	}
	else {
	    $shortSpecies = $species;
	    $shortSpecies =~ s/\s+/_/g;
	}
    }
    elsif($species=~/(\S+)\s+(\S+)/){
	$shortSpecies = substr($1,0,1) . "." . $2; 
    }
    else {
	$shortSpecies = $species;
    }
    
    return $shortSpecies;
}
######################################################################
#
sub tax2kingdom {
    my $species = shift;
    my $kingdom;
    #unclassified sequences; metagenomes; ecological metagenomes.
    if ($species=~/^(.+?);\s+(.+?)\.*?;/){
	$kingdom = "$1; $2";
    }
    die "FATAL: failed to parse a kingdom from species string: [$species]. email pg5!" if not defined $kingdom;
    
    return $kingdom;
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
	    -v|--verbose                 print loads of cruft
	    -h|-help                     Print this help

To add:
-add a warning if a seed sequence matches a forbidden term
-Markup up highest scoring representative for each species?
-add source eg. EMBL;STD;ENV to out.list &/or species?
-add a check for overlaps with existing families -- add to the seqLabel column?
-print threshold label when there are no low scoring hits...
-shortSpeciesName fails on AAXH01001154.1 (RF01269)
-account for the extent of overlap!
-write nse2array & nvse2array functions!

I was happily ignorant of this until recently!:
4:M10217.1/5840-5910    5770-5840 OVERLAPS!
4:M10217.1/5840-5910    5841-5909 OVERLAPS!
4:M10217.1/5840-5910    5910-5978 OVERLAPS!

[0] [7] replace M10217.1/5909-5841 with M10217.1/5910-5840? [pid=100.00 cover=100.00] [blat=135.0 bits=54.91 E=3.67e-07] [.] [X.laevis] [EMBL;STD;VRT:Xenopus laevis mitochondrial DNA, complete genome.]

-PROFILE THE CODE -- SEEMS TO SPEND AGES ON FETCHING TAXON NAMES...
-USE THE SAME CMALIGN MODE AS FOR CMSEARCH -- EG. IF LOCAL USE LOCAL, IF GLOCAL USE GLOBAL

EOF
}
