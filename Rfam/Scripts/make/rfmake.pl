#!/software/bin/perl -w

#NOWTODO: put variable decalarations at top of block (PRELIMINARIES, PARSE TABFILE...)
# TODO: put rfsearch list mode code into rfsearch
# TODO: abstract out creation of out.list into subroutine, it will get called from rfsearch and rfmake

#rfmake.pl - a script designed to process the results of rfsearch.pl. 
# $Id: rfmake.pl,v 1.71 2013-01-23 10:06:59 en1 Exp $

use strict;
use Cwd;
use Getopt::Long;
use File::Copy;

use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::Family::MSA;
use myrfam; #TODO: put these functions into a new Rfam module

use Data::Printer;

my $config = Bio::Rfam::Config->new;

###################################################################
# PRELIMINARIES:
# - set default values that command line options may change
# - process command line options
# - set input/output file names, and ensure input files exist
# - process DESC file

# set default values that command line options may change
my $do_thr = 0;       # TRUE to run in THRESHOLD mode, TRUE only if -t used
my $do_list = 1;      # TRUE to run in LIST mode, FALSE if -t used
my $do_align = 0;     # TRUE to create align file
my $do_subalign = 0;  # TRUE to create SUBALIGN
my $do_help = 0;      # TRUE to print help and exit, if -h used
my $do_taxinfo = 1;   # TRUE to create taxinfo file, FALSE if --notaxinfo
my $do_compare = 1;   # TRUE to create comparison file, FALSE if --nocompare
my $dirty = 0;        # TRUE to leave files on file system
my $thr;              # threshold, only defined if -t used
my $dbchoice = "rfamseq";
my $farm = 0;         # TRUE to use farm for alignment jobs
my $queue       = ""; # queue choice
my @cmosA;            # extra single - cmalign options (e.g. -g)
my @cmodA;            # extra double - cmalign options (e.g. --cyk)


&GetOptions( "l"          => \$list,
	     "t=s"        => \$thr,
	     "e=s"        => \$evalue,
	     "a",         => \$do_align,
	     "subalign"   => \$do_subalign,
	     "queue=s"    => \$queue,
	     "farm"       => \$farm,
	     "dbchoice=s" => \$dbchoice,
             "cmos=s@"    => \@cmosA,
             "cmod=s@"    => \@cmosD,
	     "notaxinfo"  => \$no_taxinfo,
	     "nocompare"  => \$no_compare,
	     "dirty"      => \$dirty,
	     "verbose"    => \$verbose,
	     "h|help"     => \$do_help );

if( $do_help ) {
    &help();
    exit(1);
}

# extra processing of command-line options 
if((defined $thr) && (defined $evalue)) { die "ERROR -t and -e combination is invalid, choose 1"; }
if((defined $thr) || (defined $evalue)) { $do_thr = 1; $do_list = 0; }
    
# setup dbfile 
$dbconfig = $config->seqdbConfig($config, $dbchoice);
$dbfile   = $dbconfig->{"path"};

# enforce -a or --subalign selected if used align-specific options used
if((! $do_align) && (! $do_subalign)) { 
    if(defined $queue)  { die "ERROR --queue requires -a or --subalign"; }
    if($farm)           { die "ERROR --farm requires -a or --subalign";  }
    if(defined \@cmosA) { die "ERROR --cmosA requires -a or --subalign"; }
    if(defined \@cmodA) { die "ERROR --cmosA requires -a or --subalign"; }
}

# set input (suffix 'I') and output (suffix 'O') file names, ensure input file names exist
my $descI    = "DESC";
my $seedI    = "SEED";
my $tabfileI = "TABFILE";
my $outlistO = "outlist";
my $speciesO = "species";
my $taxinfoO = "taxinfo";
my $rinO     = "rin.dat";
my $rincO    = "rincounts.dat";

foreach $file ($descI, $seedI, $tabfileI) { 
    # write function: check for required input file, which should be able to take multiple input files
    if(! -s $file) { die "ERROR: required file $file does not exist or is empty\n"; }
}

# TODO: find out if umask(002) is necessary
# make sure files are writable by group
umask(002);

if(defined $evalue) { 
  # TODO: determine minimum bit score with E-value <= $evalue, set as $thr, 
   # use CM's e-value parameters and dbsize
    die "ERROR rfmake.pl not yet set up for -e E-value threshold, write code using CM E-value parameters DBSIZE etc...";
}

######################################################################
# Make hashes for checking overlaps with seeds:
# NOWTODO: check over this code, use Easel MSA module and probably make into >=1 subroutines

my (%seedseqs_start, %seedseqs_end, %seedseqs_strand, %seedseqs_found);
my $sqacc;  # sequence accession
my $start;  # hit start
my $end;    # hit end
my $a;      # min of start, end
my $b;      # max of start, end
my $strand; # 1 if $start <= $end, else -1
my $msa = Bio::Rfam::Family::MSA->new({
    fileLocation => $seedI,
    aliType => 'seed'
});
my $nseq = $msa->nseq();
for($i = 0; $i < $nseq; $i++) { 
    $sqname = $msa->get_sqname_idx($i);
    if($sqname =~ m/^(\S+)\/(\d+)\-(\d+)\s*/) {
	($sqacc, $start, $end) = ($1,$2,$3);
	$seedseqs_found{$sqname} = 0;
	if($s <= $e) { $strand =  1; $a = $start; $b = $end;   }
	else         { $strand = -1; $a = $end;   $b = $start; }
	push(@{$seedseqs_start{$n}}, $a); 
	push(@{$seedseqs_end{$n}},   $b);  
	push(@{$seedseqs_strand{$n}},$strand);     
    } 
    else { 
	# SEED sqname of unexpected format
	# I think we should possibly handle this (new families), but die for now
	die "ERROR SEED sequence name of unexpected format $sqname"; 
    }
}


# END of PRELIMINARIES block
######################################################################
# GIANT if statement (TODO: get rid of this, this will go into rfsearch.pl)
if($do_list) { 
   # Parse TABFILE block
   # define variables we'll need
   my $prv_bits   = 99999.00;     # previous bit score seen
   my $prv_evalue = 0.;           # previous E-value seen
   my $rfdbh = $db->storage->dbh; # database connection
   my %kingdomCounts;             # counts of hits in each kingdom
   my %store;                     # stores N/S-E's for overlap checks
   my $printed_thresh;            # TRUE if threshold has already been printed
   my $thrcurr;                   # current threshold, if defined in DESC

   # process desc file to get GA cutoff
   my $io   = Bio::Rfam::FamilyIO->new;
   my $desc = $io->parseDesc($descI);
   if(defined $desc->CUTGA) { $thrcurr = $desc->CUTGA; }
   else                     { $thrcurr = -1000; }

   # db queries:
   # query to search for the description of embl entries with the embl id
   my $queryDesc = qq( 
           select description
                   from rfamseq where rfamseq_acc=? and version=?;         
   );
   # query to fetch the species name, full taxonomy string and ncbi id for a given embl id:
   my $queryTax = qq(
           select t.species, t.tax_string, t.ncbi_id 
           from taxonomy as t, rfamseq as r 
           where t.ncbi_id=r.ncbi_id and r.rfamseq_acc=?;
   );
   
   # Prepare the queries for execution.
   # TODO: potentially refactor to use DBX class
   my $sthDesc = $rfdbh->prepare($queryDesc);
   my $sthTax = $rfdbh->prepare($queryTax);
   
   # open output files
   rename("$outlistO", "$outlist.old")  if -e $outlistO;
   rename("$speciesO", "$speciesO.old") if -e $speciesO;

   open(OUT, "> $outlistO") || die "FATAL: failed to open $outlistO)\n[$!]";
   printf OUT "# %0.4s\t%0.12s\t%s\t%0.20s\t%10s\t%10s\tqStart\tqEnd\t%0.20s\t%0.70s\n", 
   'bits', 'evalue', 'seqLabel', 'name', 'start', 'end', 'mstart', 'mend', 'trunc', 'shortSpecies', 'description';    
   
   open(SPC,"> $speciesO") || die "FATAL: failed to open $speciesO\n[$!]\n";   
   printf SPC "# %0.4s\t%0.12s\t%0.6s\t%0.20s\t%0.15s\t%0.20s\t%s\n", 
   'bits', 'evalue', 'seqLabel', 'name', 'ncbiId', 'species','taxString';
   
   open(RIN,"> $rinO") || die "FATAL: failed to open $rinO\n[$!]\n";   
   printf RIN "bits\ttype\ttrueOrFalse\ttax\n";

   open(RINc,"> $rincO") || die "FATAL: failed to open rincO\n[$!]\n";   
   printf RINc "cnt\ttax\n";

   # TODO: don't use grep and sort, use PERL's sort, even though paul wrote this:
   # If you don't like this you can fuck off!:
   # Shell grep & sort are a hell of a lot less resource greedy than perl's equivalents.

   # actually parse tabfile
   open(TAB, "grep -v ^'#' $tabfileI | sort -nrk 15 | ") or die "FATAL: could not open pipe for reading $tabfileI\n[$!]";
   my $tabline;
    while ($tabline = <TAB>){
	
      # Infernal 1.1 example:  ABEF01002040.1       -         RF01714              -          cm        1       72      258      187      -    no    1 0.49   0.0  102.6   1.1e-19 !   Marine metagenome HOTS_Contig2040, whole genome shotgun sequence.
      #                        0                    1         2                    3          4          5      6        7        8       9    10   11 12     13   14      15
      if($tabline !~ m/^\#/) { 
	  my @tabA = split(/\s+/, $tabline);
	  my ($name, $qStart, $qEnd, $start, $end, $strand, $trunc, $bits, $evalue) = ($tabA[0], $tabA[5], $tabA[6], $tabA[7], $tabA[8], $tabA[9], $tab[10], $tabA[14], $tabA[15])
	  if($strand eq "+") { $strand = 1; } else { $strand = -1; }
	  #print "$name,$start,$end,$strand\n";

	  # NO OVERLAPS IN INFERNAL -- WE DON'T VALIDATE THIS
	  push( @{ $store{$name} }, { 'start'  => $start,
				      'end'    => $end
				      } );
	  
	  #Fetch info from the DB:
	  #TODO check over this block, make it into a function? (is this difficult due to database calls?)
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
	      $domainKingdom = tax2kingdom($taxString . '; ' . $species . ';');
	      $kingdomCounts{$domainKingdom}=0 if not defined $kingdomCounts{$domainKingdom};
	      $kingdomCounts{$domainKingdom}++;
	  }
	  
	  $description  = 'NA' if not defined $description;
	  $species      = 'NA' if not defined $species;
	  $shortSpecies = 'NA' if not defined $shortSpecies;
	  
	  # determine seqLabel
	  my $seqLabel = 'FULL';
	  my $seqLabelExtent = '';
	  my $overlapExtent = overlapsSeed($name,$start,$end,\%seedseqs_start, \%seedseqs_end, \%seedseqs_strand, \%seedseqs_found);
	  if ($overlapExtent>0.1){ # Must overlap significantly to be considered:
	      $seqLabel = 'SEED';
	      $seqLabelExtent = sprintf ".%d", int(100*$overlapExtent);
	  }
	  if (($bits < $thrcurr) && ($seqLabel ne 'SEED')) { 
	      $seqLabel    = 'NOT' 
	  }

	  # print out threshold line if nec
	  if ( $bits < $thrcurr && $thrcurr<=$prevBits){
	      $outline = "#***********CURRENT THRESHOLD: $thrcurr bits***********#\n";
	      print OUT $outline;
	      print SPC $outline;
	      printf RIN  "%0.2f\tTHRESH\t\.\t.\n", $thrcurr;
	      $printedThresh=1;
	  }
	  $prevBits = $bits;

	  if ($evalue > 0.0001 && $prev_evalue <= 0.0001) { 
	      $outline = "#***********E-VALUE OF 1E-4***********#\n";
	      print OUT $outline;
	      print SPC $outline;
	  }
	  
	  if ($evalue > 1 && $prev_evalue <= 1) { 
	      $outline = "#***********E-VALUE OF 1**************#\n";
	      printf OUT  $outline;
	      printf SPEC $outline;
	  }
	  $prev_evalue = $evalue;
	  
	  printf OUT  "%0.2f\t%0.12s\t%0.6s\t%0.20s\t%10d\t%10d\t$%10d\t%10d\t%2d\t%0.20s\t%0.70s\n", $bits, $evalue, $seqLabel, $name, $start, $end, $qstart, $qend, $trunc, $shortSpecies, $description;
	  printf SPEC "%0.2f\t%0.12s\t%0.6s\t%0.20s\t%15d\t%0.20s\t%s\n", $bits, $evalue, $seqLabel, $name, $ncbiId, $species,$taxString;
	  printf RIN  "%0.2f\t%0.6s\t$$domainKingdom\n", $bits, $seqLabel;
	  
      } # closes 'if($tabline =~ m/^\#/) {'
    } # closes 'while($tabline = <TAB>)'
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
   # TODO: put path to R in config, update plot_outlist.R so it doesn't expect termLabels, then uncomment next line
   # and update path with config variable
   #system("/software/R-2.9.0/bin/R CMD BATCH --no-save /software/rfam/bin/plot_outlist.R") and warn "WARNING: system call for /software/R-2.9.0/bin/R failed. Check binary exists and is executable.\n[/software/R-2.6.0/bin/R CMD BATCH --no-save ~pg5/scripts/make/plot_outlist.R]\n";
    
    #Complain loudly if seed sequences are missing from the output:
    foreach my $n (keys %seedseqs_found){
	if ($seedseqs_found{$n}<1){
	    printf          "WARNING: SEED sequence $n was not in the OUTPUT!\n";
	    push(@warnings, "WARNING: SEED sequence $n was not in the OUTPUT!\n");
	}
    }

    # TODO: hook taxinfo, subalign and comparison back up, look at make/orig-files/rfmake.pl for reference,
    # I've deleted all the code here.

    open( WARN, ">warnings" ) or warn "Can't open warnings files\n";
    foreach my $w (@warnings){
	print WARN $w;
    }
    close(WARN);
} # end of if($do_list) end of LIST mode block
######################################################################
# THRESHOLD mode
else { 
    # check for required files
    my $outlistI = "outlist";
    foreach $file ($outlistI) { 
	if(! -s $file) { die "ERROR: required file $file does not exist or is empty\n"; }
    }
    if(! defined $thr) { 
	die "ERROR: threshold is not defined"; 
    }

    make_scores($outlistI, $thr, \@scoresAA);
    #NOWTODO 

    if($do_align) { 
	# make sure we have a valid SCORES file
	# TODO: replace this with FamilyIO
	$sfetch = $config->easelPath;
	if(! -s SCORES) { die "ERROR SCORES does not exist to supply to esl-sfetch"; }
	eslSfetch_Cf($sfetch, $dbfile, "SCORES", "$$.fa");

        # use cmalign to do the alignment
	my $options = "";
	if (@cmosA){#covers the '-' options
	    foreach my $opts (@cmosA){
		$options .= " \-$opts ";
	    }
	}
	if (@cmodA){#covers the '--' options 
	    foreach my $opts (@extraCmalignOptionsDouble){
		$options .= " \-\-$opts ";
	    }
	}
        # Run cmalign locally or on farm (autodetermined based on job size, unless -a or -n used)
	cmAlign("CM", "$$.fa", "align", "cmalign.out", $options, $nseq, $tot_len, ($farm), (! $farm), $queue, $dirty);

        # remove temporary fasta file
	if(! $dirty) { unlink "$$.fa"; }
    } # end of if($do_align)

    # update DESC
    my $io   = Bio::Rfam::FamilyIO->new;
    my $desc = $io->parseDesc($descI);
    my ($tc_bits, $nc_bits) = findTcNc( $thr, $outlistO);
    $desc->{GA}($thr);
    $desc->{TC}($tc_bits);
    $desc->{NC}($nc_bits);
    $io->writeDesc($desc);

    copy('DESC', $$ . '.DESC') if -e 'DESC';
}
exit 0;


###########################
# SUBROUTINES

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

##########################

sub tax2kingdom {
    my ($species) = @_;
    my $kingdom;
    #unclassified sequences; metagenomes; ecological metagenomes.
    if ($species=~/^(.+?);\s+(.+?)\.*?;/){
	$kingdom = "$1; $2";
    }
    die "FATAL: failed to parse a kingdom from species string: [$species]. email pg5!" if not defined $kingdom;
    
    return $kingdom;
}

######################################################################
# threshold_outlist: create scoresAA and esl-sfetch input file for sequence fetching
sub make_scores {
    my ($outlistI, $threshold, $scoresAAR) = @_;
    my $nseq    = 0;
    my $tot_len = 0;

    my $name;
    my $tcode;
    while (<OL>){
	if(! m/^\#/) { 
	    my @elA = split(/\t/);
	    # example line
	    # 89.10	1e-15	ALIGN	ACFV01131345.1	     33633	     33533	1	101    no  Callithrix_jacchus_w	Callithrix jacchus Contig363.22, whole genome shotgun sequence.
	    # 0     1       2       3                    4               5          6       7       8  9   
	    my ($bits, $evalue, $id, $start, $end, $qstart, $qend, $tstr) = ($elA[0], $elA[1], $elA[3], $elA[4], $elA[5], $elA[6], $elA[7], $elA[9]);
	    if($bits < $threshold) { last; }
	    
	    $name  = "$id/$start-$end";
	    $tcode = truncString2Code($tstr);
	    push(@{$scoresAAR->[$n]}, ($name, $start, $end, $id, $bits, $evalue, $qstart, $qend, $tcode));
	    $n++;
	}
    }
    p($scoresAAR);
    close(OL);

    return (\%scores, $nseq, $tot_len);
}
#########################################################
sub truncString2Code {
    my $str = @_[0];
    if($str eq "no")    { return 0;  }
    if($str eq "5'")    { return 5;  }
    if($str eq "3'")    { return 3;  }
    if($str eq "5'&3'") { return 53; }
}

######################################################################
sub help {
    print STDERR <<EOF;
    
rfmake.pl - process the results of rfsearch.pl. 
  TODO: overhaul rfmake.pl so it only runs in 'threshold' mode, put 'list' mode functionality in rfsearch,
        then rewrite synopsis immediately below:
          - rfmake.pl processes OUTPUT files from rfsearch/cmsearch. In the first round \42-l\42
	    is used to produce a \42out.list\42 file that contains a tabular summary of the hits,
	    based upon this output, the score distributions in out.list.pdf \& out.list.trunc.pdf 
	    and the MCC plot in out.list_accuracy.dat the CURATOR chooses a threshold with which 
	    a full ALIGNment is built using the \42-t <bits>\42 option. 

Usage:      rfmake.pl -l          TODO: add this functionality to cmsearch
	    rfmake.pl -t <bits> 

Options:    MAJOR MODES:
    	    -l      lists hits only, don\'t apply threshold [default]
	    -t <f>  don\'t list hits, set threshold as bit score <f>
	    -e <f>  don\'t list hits, set threshold as minimum integer bit score w/E-value <= <f>
	    
	    OPTIONS RELATED TO CREATING ALIGNMENTS (by default none are created):
	    -a           create 'align' alignment with all hits above threshold, with cmalign (requires -t or -e)
	    --subalign   create 'subalign' alignment, with sampling of representative hits
            --queue <s>  submit jobs to queue <s> (<s>='production-rh6' (default), 'research-rh6')
 	    --farm       always run cmalign on the farm [default: don't, run locally]
	    --cmos <str> add extra arbitrary option to cmalign with '-<str>'. (Infernal 1.1, only option is '-g')
            --cmod <str> add extra arbitrary options to cmalign with '--<str>'. For multiple options use multiple
	                 -cmod lines. Eg. '-cmod cyk -cmod sub' will run cmalign with --cyk and --sub.

	     OTHER:
	     --nocompare  do not create COMPARISON file
 	     --notaxinfo  do not create TAXINFO file
	     --dirty      leave temporary files, don't clean up
  	     --verbose    print loads of cruft
  	     -h|-help     print this help, then exit

EOF
}
