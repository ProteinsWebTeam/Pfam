#!/software/bin/perl -w

#rfmake.pl - a script designed to process the results of rfsearch.pl. 
# $Id: rfmake.pl,v 1.71 2013-01-23 10:06:59 en1 Exp $

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
    $dbfile,
    $list,
    $help, 
    $file,
    $global,
    @extrafamily,
    @extra_forbidden_terms,
    @taxonomy,
    @forbiddentaxonomy,
    @forbiddentaxonomynot,
    $output,
    $incldb,
    @warnings,
    $verbose,
    @extraCmalignOptionsSingle,
    @extraCmalignOptionsDouble,
    $huge,
    $no_taxinfo,
    $no_compare);

my $always_farm = 0;  # TRUE to always use farm for alignment jobs
my $never_farm  = 1;  # TRUE to never use farm for alignment jobs (True by default)
my $queue       = ""; # queue choice
my $dirty       = 0;  # TRUE to leave files on file system
my $do_subalign = 0;  # TRUE to create SUBALIGN

&GetOptions( "t=s"      => \$thr,
	     "db=s"     => \$dbfile,
	     "l"        => \$list,
	     "file=s"   => \$file,
	     "g|global" => \$global,
	     "efam|extrafamily=s@"      => \@extrafamily,
	     "efor|extraforbidden=s@"   => \@extra_forbidden_terms,
	     "rt|taxonomy=s@"           => \@taxonomy,
	     "ft|forbiddentaxonomy=s@"  => \@forbiddentaxonomy,
	     "ftn|forbiddentaxonomynot=s@"  => \@forbiddentaxonomynot,
	     "o|output=s"     => \$output,
             "incldb=s"       => \$incldb,
             "cmos|cmalignoptions=s@" => \@extraCmalignOptionsSingle,
             "cmod|cmalignoptiond=s@" => \@extraCmalignOptionsDouble,
	     "huge"                   => \$huge,
	     "subalign"       => \$do_subalign,
	     "notaxinfo"      => \$no_taxinfo,
	     "nocompare"      => \$no_compare,
	     "dirty"          => \$dirty,
	     "v|verbose"      => \$verbose,
	     "farm"           => \$always_farm,
	     "queue=s"        => \$queue,
	     "h|help"         => \$help );

if( $help ) {
    &help();
    exit(1);
}

# make sure files are writable by group
umask(002);

#Setting/getting io files:
my $user_set_dbfile = 0;
if (defined $dbfile) { 
    $user_set_dbfile = 1;
}
else { 
    $dbfile = $Rfam::rfamseq_for_sfetch;
}
$output = "out.list" if not defined $output;

if($always_farm) { 
    $never_farm = 0; 
}
######################################################################
#DESC file:
my $thrcurr = 5;
#Validate SEED and DESC files
my $desc;
if (-s 'DESC'){
    $desc = RfamUtils::slurpDesc();
    $thrcurr = $desc->{'GA'} if defined $desc->{'GA'} && RfamUtils::isNumeric($desc->{'GA'});
# We no longer do global alignment (unless rfmake.pl -g) with Infernal 1.1    
}
else {
    $desc = RfamUtils::generateDesc();
}

$list = 1 if not defined $thr;
my (@family_terms, %family_terms, @forbidden_terms);
my (%seedseqs_start,%seedseqs_end, %seedseqs_strand, %seedseqs_found, %seen_id);
if ($list){
    
#Make into a function:
######################################################################
#Sorting out forbidden terms, family terms, etc.
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
    print "Using family terms: [@family_terms]\n" if (defined $verbose);
    @forbidden_terms = @Rfam::forbidden_terms;
    if (@extra_forbidden_terms){
	push(@forbidden_terms, @extra_forbidden_terms);
    }
    print "Using forbidden terms: [@forbidden_terms]\n" if (defined $verbose);

#Make into a function:
######################################################################
#Make hashes for checking overlaps with seeds:
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

######################################################################

$file = "TABFILE" if not defined($file);
die "FATAL: $file either doesn't exist or is empty!\n" if not -s $file;

my $prevBits    = 99999.00;
my $prev_evalue = 0.;
    
    
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
    
    
    my %store; #Used to store N/S-E's for overlap checks
    my $printedThresh;

    #Paul's comment: If you don't like this you can fuck off!:
    # Infernal 1.0 format: open(F, "grep -v ^'#' $file | sort -k6nr | ") or die "FATAL: could not open pipe for reading $file\n[$!]";
    # Infernal 1.1 format: 
    open(F, "grep -v ^'#' $file | sort -nrk 15 | ") or die "FATAL: could not open pipe for reading $file\n[$!]";
    #Shell grep & sort are a hell of a lot less resource greedy than perl's equivalents.
  TABFILE: while (my $tabline = <F>){
      # Note differences b/t 1.0 and 1.1 format here. 1.0 had start-end b/c it used blast minidbs.
      # Infernal 1.0 example:  ABEF01002814.1/664-782:1                   18          90      1     72     92.54  1.41e-17   38
      # Infernal 1.0 format:   if($tabline=~/(\S+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+\.\d+)\s+(\S+)\s+\d+/){
      # Infernal 1.1 example:  ABEF01002040.1       -         RF01714              -          cm        1       72      258      187      -    no    1 0.49   0.0  102.6   1.1e-19 !   Marine metagenome HOTS_Contig2040, whole genome shotgun sequence.
      # Infernal 1.1 format: 
      if($tabline !~ m/^\#/) { 
	  my @tabA = split(/\s+/, $tabline);
	  my ($name, $qStart, $qEnd, $start, $end, $strand, $bits, $evalue) = ($tabA[0], $tabA[5], $tabA[6], $tabA[7], $tabA[8], $tabA[9], $tabA[14], $tabA[15]);
	  if($strand eq "+") { $strand = 1; } else { $strand = -1; }
	  #print "$name,$start,$end,$strand\n";
	  # no need to recompute boundaries like we did with blast mini dbs

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
	      $shortSpecies  = RfamUtils::species2shortspecies($species);
	      $domainKingdom = tax2kingdom($taxString . '; ' . $species . ';', $huge);
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

	  if ($evalue > 0.0001 && $prev_evalue <= 0.0001) { 
	      printf OUT  "#***********E-VALUE OF 1E-4***********#\n";
	      printf SPEC "#***********E-VALUE OF 1E-4***********#\n";
	  }
	  
	  if ($evalue > 1 && $prev_evalue <= 1) { 
	      printf OUT  "#***********E-VALUE OF 1**************#\n";
	      printf SPEC "#***********E-VALUE OF 1**************#\n";
	  }
	  $prev_evalue = $evalue;
	  
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
    system("/software/R-2.9.0/bin/R CMD BATCH --no-save /software/rfam/bin/plot_outlist.R") and warn "WARNING: system call for /software/R-2.9.0/bin/R failed. Check binary exists and is executable.\n[/software/R-2.6.0/bin/R CMD BATCH --no-save ~pg5/scripts/make/plot_outlist.R]\n";
    
    #Complain loudly if seed sequences are missing from the output:
    foreach my $n (keys %seedseqs_found){
	if ($seedseqs_found{$n}<1){
	    printf          "WARNING: SEED sequence $n was not in the OUTPUT!\n";
	    push(@warnings, "WARNING: SEED sequence $n was not in the OUTPUT!\n");
	}
    }

    ################################################
    # Create TAXINFO, SUBALIGN, and COMPARISON files
    # EPN 12.20.12
    ################################################

    my $cmd;
    my $opts = "";

    # Call taxinfo.pl to create TAXINFO
    unless (defined $no_taxinfo) { 
	$cmd = "taxinfo.pl > /dev/null";
	printf("Running command $cmd ... ");
	system($cmd);
	printf("done.\n");
	if($? != 0) { die "Command $cmd failed"; }
    }

    # Call subalign.pl to create SUBALIGN 
    if($do_subalign) { 
	if($always_farm) { $opts .= " --farm"; }
	if($queue ne "") { $opts .= " --queue $queue"; }
	if($dirty)       { $opts .= " --dirty"; }
	$cmd = "subalign.pl $opts";
	printf("Running command: $cmd\n");
	system($cmd);
	printf("done.\n");
	if($? != 0) { die "Command $cmd failed"; }
    }

    # Call compare_searches.pl multiple times to create COMPARISON
    #bad hardcoded path- as this is a temporary location-
    #should be ok for the farm now..
    unless (defined $no_compare) { 
	printf("Running compare_searches.pl ... ");
	my $rcs_lustre="/lustre/scratch109/sanger/rfam-pipe/CURRENT";
	if(-d $rcs_lustre) { 
	    my $f= getcwd;
	    $f=~s/^.*\///g;
	    if(-d "$rcs_lustre/$f") { # if we're building a new family $rcs_lustre/$f won't exist
		my $oldcm="$rcs_lustre/$f/CM";
		my $grep=`grep 1.1rc $oldcm`;
		if($grep) { 
		    printf          "WARNING: COMPARISON file not created because $rcs_lustre already updated with Infernal 1.1 results\n";
		    push(@warnings, "WARNING: COMPARISON file not created because $rcs_lustre already updated with Infernal 1.1 results\n");
		}
		else { # do the comparison between BLAST + Infernal 1.0 versus Infernal 1.1
		    $cmd = "compare_searches.pl -E 1E-5 > COMPARISON";
		    system($cmd);
		    if($? != 0) { die "Command $cmd failed"; }
		    
		    my @eA = ("1E-4", "0.001", "0.01", "0.1", "1"); 
		    foreach my $e (@eA) { 
			$cmd = "compare_searches.pl -q -E $e >> COMPARISON";
			system($cmd);
			if($? != 0) { die "Command $cmd failed"; }
		    }
		}
	    }
	    else { 
		printf          "WARNING: COMPARISON file not created because $rcs_lustre/$f does not exist (new family?)\n";
		push(@warnings, "WARNING: COMPARISON file not created because $rcs_lustre/$f does not exist (new family?)\n");
	    }
	}
	else { 
	    printf          "WARNING: COMPARISON file not created because $rcs_lustre does not exist\n";
	    push(@warnings, "WARNING: COMPARISON file not created because $rcs_lustre does not exist\n");
	}
	printf(" done\n");
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

# Fetch sequences 
# as of 01.18.13, we only have one database for fetching ($dbfile), 
# either set by user with -d or else defined as current full rfamseq FA file

my ($scores, $nseq, $tot_len) = outlist2sfetchInput($output, $thr);
printf STDERR ("Fetching %d seqs from $dbfile ... ", $nseq);
my $start_time = time();
Rfam::RfamSearch::eslSfetch_Cf($dbfile, "$$.sfetch", "$$.fa");
# remove sfetch file
if(! $dirty) { unlink "$$.sfetch"; }

my $end_time = time();
printf STDERR ("done [%d seconds; %d total residues]\n", ($end_time-$start_time+1), $tot_len);

# use cmalign to do the alignment

my $options = "";
# 1.1: local is default, 1.0: global is default
if( defined $global ) {
    $options = " -g ".$options;
}

if (@extraCmalignOptionsSingle){#covers the '-' options
    foreach my $opts (@extraCmalignOptionsSingle){
	$options .= " \-$opts ";
    }
}

if (@extraCmalignOptionsDouble){#covers the '--' options 
    foreach my $opts (@extraCmalignOptionsDouble){
	$options .= " \-\-$opts ";
    }
}

# Run cmalign locally or on farm (autodetermined based on job size, unless -a or -n used)
Rfam::RfamSearch::cmAlign("CM", "$$.fa", "ALIGN", "cmalign.out", $options, $nseq, $tot_len, $always_farm, $never_farm, $queue, $dirty);
# remove temporary fasta file
if(! $dirty) { unlink "$$.fa"; }

# print warnings if scores are too low, or if seq in ALIGN not in scores file
open (CMA, "cmalign.out") or die "FATAL: failed to open cmalign.out\n[$!]";
my $alignTime = 0;
while (my $cma = <CMA>){
    if($cma =~ /^\s+\d+\s+(\S+)\s+\d+\s+\d+\S+\s+([-]?\d+\.\d+)/){
	#Print warning if cmalign bit score is significantly different from the cmsearch bit score?
	if(defined($scores->{$1}) and 0.9*($scores->{$1}) > $2 ){
	    chomp($cma);
	    print          "WARNING: your cmalign score [$2] is significantly lower than the cmsearch score [$scores->{$1}]:\n\ttry re-running rfmake.pl with \42-cmod sums\42\n";
	    push(@warnings,"WARNING: your cmalign score [$2] is significantly lower than the cmsearch score [$scores->{$1}]:\n\ttry re-running rfmake.pl with \42-cmod sums\42\n");    
	}
	elsif(not defined($scores->{$1})){
	    print          "WARNING: [$1] is in ALIGN but is not in the \42scores\42 file!\n";
	    push(@warnings,"WARNING: [$1] is in ALIGN but is not in the \42scores\42 file!\n");    
	}
    }
    if($cma =~ /\#\s+CPU\s+time\:\s+(\S+)u\s+(\S+)s/){
	$alignTime=$1+$2;
    }
}
close(CMA);

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
$runTimes{'cmalign'} = $alignTime;

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
	print "term [$t] matches [$description]\n" if (defined $verbose && $description =~ /$t/);
	return 1 if ($description =~ /$t/i);
    }
    return 0;
}

######################################################################
#outlist2fetchSeqsHashes: create %forward & %reverse hashes of arrays of hashes for sequence fetching
sub outlist2fetchSeqsHashes {
    my ($outlist,$threshold) = @_;
    open(OL, "< $outlist") or die "FATAL: failed to open $outlist\n[$!]";
    open( SC, ">scores" )  or die "FATAL: failed to open scores\n[$!]";
    open( SCE, ">scores.evalue" )  or die "FATAL: failed to open scores.evalue\n[$!]";
    my (%forward, %reverse, %scores);
    while (<OL>){
	if(/(\S+)\s+(\S+)\s+(SEED\.\d+|SEED|ALIGN|NOT)\s+(\S+)\s+(\d+)\s+(\d+)/){
	    my ($bits,$evalue,$id,$start,$end)=($1,$2,$4,$5,$6);
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
	    $scores{"$id/$start-$end"}=$bits;
	    print SCE "$bits $evalue $id/$start-$end\n";
	}
    }
    close(OL);
    close(SC);
    
    return (\%forward, \%reverse, \%scores);
}

######################################################################
#
sub tax2kingdom {
    my ($species, $huge) = @_;
    my $kingdom;
    #unclassified sequences; metagenomes; ecological metagenomes.
    if ($species=~/^(.+?);\s+(.+?)\.*?;/){
	$kingdom = "$1; $2";
	$kingdom = $1 if defined $huge;
    }
    die "FATAL: failed to parse a kingdom from species string: [$species]. email pg5!" if not defined $kingdom;
    
    return $kingdom;
}

######################################################################
# outlist2sfetchInput: create esl-sfetch input file for sequence fetching
sub outlist2sfetchInput {
    my ($outlist,$threshold) = @_;
    my $nseq    = 0;
    my $tot_len = 0;
    open( OL,  "< $outlist")       or die "FATAL: failed to open $outlist\n[$!]";
    open( SC,  ">scores" )         or die "FATAL: failed to open scores\n[$!]";
    open( SCE, ">scores.evalue" )  or die "FATAL: failed to open scores.evalue\n[$!]";
    open( SF,  ">$$.sfetch" )      or die "FATAL: failed to open $$.sfetch\n[$!]";
    my (%scores);
    my $longname;
    while (<OL>){
	if(/(\S+)\s+(\S+)\s+(SEED\.\d+|SEED|ALIGN|NOT)\s+(\S+)\s+(\d+)\s+(\d+)/){
	    my ($bits,$evalue,$id,$start,$end)=($1,$2,$4,$5,$6);
	    last if $bits < $threshold;
	    
	    $longname = "$id/$start-$end";
	    print SF ("$longname $start $end $id\n");
	    print SC $bits, " $longname\n";
	    $scores{"$longname"}=$bits;
	    print SCE "$bits $evalue $longname\n";

	    $nseq++;
	    if($end > $start) { $tot_len += ($end-$start+1); }
	    else              { $tot_len += ($start-$end+1); }
	}
    }
    close(OL);
    close(SC);
    close(SCE);
    close(SF);
    
    return (\%scores, $nseq, $tot_len);
}
#########################################################

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
	    -t <bits>                    Make a full alignment, using <bits> as the threshold for inclusion
    	    -l                           option lists hits but does not build ALIGN
	    -g|--glocal                  Run cmalign global mode (DESC cmsearch command is always ignored!)
	    
            I/O
	    -file <infernal output file> Use an alternative cmsearch output [Default: OUTPUT].
	    --db <db>                    Use a different database for sequence fetching.
	    -incldb                      Include an additional directory of databases
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
	    -huge                        Employ some tricks for HUGE families.
             CMALIGN:					 
	     -cmos|--cmalignoptions <str> Add extra arbitrary options to cmalign with a single '-'. For multiple options use multiple 
	                                     -cmos lines. Eg. '-cmos q -cmos p' will run cmalign in quiet mode and append posterior 
					     probabilities to alignment.
	     -cmod|--cmalignoptiond <str> Add extra arbitrary options to cmalign with a double '-'. For multiple options use multiple 
	                                     -cmod lines. Eg. '-cmod cyk' will run cmalign with the CYK algorithm.
	     OTHER:
	    --subalign                  create SUBALIGN file by calling subalign.pl
	    --nocompare                 do not create COMPARISON file
	    --notaxinfo                 do not create TAXINFO file
	    --farm                      always run cmalign on the farm [default: don't, run locally]
	    --dirty                     leave temporary files, don't clean up
            --queue <s>                 submit all jobs to queue <s> (<s>=small, normal, long, basement, bigmem)
	    -v|--verbose                print loads of cruft
	    -h|-help                    Print this help


To add:
-add a warning if a seed sequence matches a forbidden term
-Markup up highest scoring representative for each species?
-add source eg. EMBL;STD;ENV to out.list &/or species?
-add a check for overlaps with existing families -- add to the seqLabel column?
-print threshold label when there are no low scoring hits...
-shortSpeciesName fails on AAXH01001154.1 (RF01269) -- Influenza A -- RF01099
-account for the extent of overlap!
-write nse2array & nvse2array functions!

I was happily ignorant of this until recently!:
4:M10217.1/5840-5910    5770-5840 OVERLAPS!
4:M10217.1/5840-5910    5841-5909 OVERLAPS!
4:M10217.1/5840-5910    5910-5978 OVERLAPS!

[0] [7] replace M10217.1/5909-5841 with M10217.1/5910-5840? [pid=100.00 cover=100.00] [blat=135.0 bits=54.91 E=3.67e-07] [.] [X.laevis] [EMBL;STD;VRT:Xenopus laevis mitochondrial DNA, complete genome.]

-PROFILE THE CODE -- SEEMS TO SPEND AGES ON FETCHING TAXON NAMES...
-PRINT WARNING IF CMALIGN BIT SCORE IS SIGNIFICANTLY DIFFERENT FROM THE CMSEARCH BIT SCORE

EOF
}
