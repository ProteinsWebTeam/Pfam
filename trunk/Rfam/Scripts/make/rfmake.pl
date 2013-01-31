#!/usr/local/bin/perl -w 

#NOWTODO: put variable decalarations at top of block (PRELIMINARIES, PARSE TBLOUT...)
# TODO: put rfsearch list mode code into rfsearch
# TODO: abstract out creation of 'list' into subroutine, it will get called from rfsearch and rfmake

#rfmake.pl - a script designed to process the results of rfsearch.pl. 
# $Id: rfmake.pl,v 1.71 2013-01-23 10:06:59 en1 Exp $

use strict;
use Cwd;
use Getopt::Long;
use File::Copy;
use Data::Printer;
use Carp;

use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::Family::MSA;
use Bio::Rfam::TempRfam;

my $config = Bio::Rfam::Config->new;
my $io     = Bio::Rfam::FamilyIO->new;
my $famObj = Bio::Rfam::Family->new(
    'SEED' => {
	fileLocation => "SEED",
	aliType      => 'seed'
    },
    'TBLOUT' => { 
	fileLocation => "TBLOUT",
    },
    'DESC'   => $io->parseDESC("DESC"),
    'CM'     => $io->parseCM("CM"),
    );
#$msa = $famObj->SEED;

## write TBLOUT's set of dependent files: "outlist", "species", "rin.dat", and "rinc.dat"
## if any of them don't exist
#if((! -s "outlist") || (! -s "species") || (! -s "rin.dat") || (! -s "rinc.dat")){ 
#    $io->writeTbloutDependentFiles($famObj->TBLOUT, );
#}

###################################################################
# PRELIMINARIES:
# - set default values that command line options may change
# - process command line options
# - set input/output file names, and ensure input files exist
# - process DESC file

# set default values that command line options may change
#TEMP 
my $do_X = 0; # set threshold as threshold from DESC TEMPORARY FOR RFAM HACKATHON

my $do_thr = 0;       # TRUE to run in THRESHOLD mode, TRUE only if -t used
my $do_list = 1;      # TRUE to run in LIST mode, FALSE if -t used
my $do_align = 0;     # TRUE to create align file
my $do_subalign = 0;  # TRUE to create SUBALIGN
my $do_help = 0;      # TRUE to print help and exit, if -h used
my $no_taxinfo = 0;   # TRUE to NOT create taxinfo file
my $no_compare = 0;   # TRUE to NOT create comparison file
my $dirty = 0;        # TRUE to leave files on file system
my $thr;              # threshold, only defined if -t used
my $dbchoice = "rfamseq";
my $farm = 0;         # TRUE to use farm for alignment jobs
my @cmosA = ();       # extra single - cmalign options (e.g. -g)
my @cmodA = ();       # extra double - cmalign options (e.g. --cyk)
my $evalue;           # E-value threshold to use, set with -e
my $verbose = 0;      # TRUE to be verbose with output
my @warningsA = ();   # array of warning messages

&GetOptions( "l"          => \$do_list,
	     "X"          => \$do_X,
	     "t=s"        => \$thr,
	     "e=s"        => \$evalue,
	     "a",         => \$do_align,
	     "subalign"   => \$do_subalign,
	     "farm"       => \$farm,
	     "dbchoice=s" => \$dbchoice,
             "cmos=s@"    => \@cmosA,
             "cmod=s@"    => \@cmodA,
	     "notaxinfo"  => \$no_taxinfo,
	     "nocompare"  => \$no_compare,
	     "dirty"      => \$dirty,
	     "verbose"    => \$verbose,
	     "h|help"     => \$do_help );

if( $do_help ) {
    &help();
    exit(1);
}

#TEMP FOR HACKATHON
if($do_X) { 
    if(defined $thr) { die "ERROR -t and -X enabled"; } 
    $do_thr = 1; 
    $do_list = 0; 
}

# extra processing of command-line options 
if((defined $thr) && (defined $evalue)) { die "ERROR -t and -e combination is invalid, choose 1"; }
if((defined $thr) || (defined $evalue)) { $do_thr = 1; $do_list = 0; }
    
# setup dbfile 
my $dbconfig = $config->seqdbConfig($dbchoice);
my $dbfile   = $dbconfig->{"path"};
my $Z     = $dbconfig->{"dbsize"};

# enforce -a or --subalign selected if used align-specific options used
if((! $do_align) && (! $do_subalign)) { 
    if($farm)              { die "ERROR --farm requires -a or --subalign";  }
    if(scalar(@cmosA) > 1) { die "ERROR --cmosA requires -a or --subalign"; }
    if(scalar(@cmodA) > 1) { die "ERROR --cmodA requires -a or --subalign"; }
}

my $outlistO = "outlist";
my $speciesO = "species";
my $taxinfoO = "taxinfo";
my $rinO     = "rin.dat";
my $rincO    = "rinc.dat";

if(defined $evalue) { 
   # TODO: use loadRfamFromLocalFile in FamilyIO.pm, but only if I can supply
   # cwd as dir
    # TODO, read SM in desc, and pick appropriate E-value line based on that
   my $cm    = $famObj->CM;
   my $bitsc = int((evalue2bitsc($cm, $evalue, $Z)) + 0.5); # round up to nearest int bit score above exact bit score
   $thr = $bitsc;
}

# Read in MSA
my $msa = Bio::Rfam::Family::MSA->new({
    fileLocation => "SEED",
    aliType => 'seed'
});
# TODO, this should automatically get called when some function that accesses gets called (lazy load, right?)
$msa->nse_createHAA();

my $idx;
my %seedseq_foundH = ();
for($idx = 0; $idx < $msa->nseq; $idx++) { 
    $seedseq_foundH{$msa->get_sqname($idx)} = 0;
}

# END of PRELIMINARIES block
######################################################################
# GIANT if statement (TODO: get rid of this, this will go into rfsearch.pl)
if($do_list) { 
   # define variables we'll need
    my $rfamdb = $config->rfamlive;
    my $rfdbh = $rfamdb->storage->dbh; # database connection
    my $prv_bits   = 99999.00;     # previous bit score seen
    my $prv_evalue = 0.;           # previous E-value seen
    my %kingdomCounts;             # counts of hits in each kingdom
    my $printed_thresh;            # TRUE if threshold has already been printed
    my $thrcurr;                   # current threshold, if defined in DESC
    my ($qstart, $qend);           # query start, query end (model start/end) from TBLOUT
    # process desc file to get GA cutoff
    if(defined $famObj->DESC->CUTGA) { $thrcurr = $famObj->DESC->CUTGA; }
    else                             { $thrcurr = -1000; }

   # db queries:
   # query to search for the description of embl entries with the embl id
   my $queryDesc = qq( 
           select description
                   from rfamseq where rfamseq_acc=?;
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
   my $sthTax  = $rfdbh->prepare($queryTax);
   
   # open output files
   rename("$outlistO", "$outlistO.old")  if -e $outlistO;
   rename("$speciesO", "$speciesO.old") if -e $speciesO;

   open(OUT, "> $outlistO") || die "FATAL: failed to open $outlistO)\n[$!]";
   printf OUT "# %0.4s\t%0.12s\t%0.6s\t%0.20s\t%10s\t%10s\t%6s\t%6s\t%0.5s\t%0.20s\t%0.70s\n", 
    'bits', 'evalue', 'seqLabel', 'name', 'start', 'end', 'qstart', 'qend', 'trunc', 'shortSpecies', 'description';    
   
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

   # actually parse tblout
    my $tbloutI = "TBLOUT";
   open(TBL, "grep -v ^'#' $tbloutI | sort -nrk 15 | ") or die "FATAL: could not open pipe for reading $tbloutI\n[$!]";
   my $tblline;
    while ($tblline = <TBL>){
	
      # Infernal 1.1 example:  ABEF01002040.1       -         RF01714              -          cm        1       72      258      187      -    no    1 0.49   0.0  102.6   1.1e-19 !   Marine metagenome HOTS_Contig2040, whole genome shotgun sequence.
      #                        0                    1         2                    3          4          5      6        7        8       9    10   11 12     13   14      15
      if($tblline !~ m/^\#/) { 
	  my @tblA = split(/\s+/, $tblline);
	  my ($name, $qstart, $qend, $start, $end, $strand, $trunc, $bits, $evalue) = ($tblA[0], $tblA[5], $tblA[6], $tblA[7], $tblA[8], $tblA[9], $tblA[10], $tblA[14], $tblA[15]);
	  if($strand eq "+") { $strand = 1; } else { $strand = -1; }
	  #print "$name,$start,$end,$strand\n";

	  #Fetch info from the DB:
	  #TODO check over this block, make it into a function? (is this difficult due to database calls?)
	  my ($description, $species, $shortSpecies, $domainKingdom, $taxString, $ncbiId);
	  if($name=~/(\S+)\.(\d+)/){
	      $sthDesc->execute($name);
	      
	      my $res = $sthDesc->fetchall_arrayref;
	      if(! defined $res) { die "ERROR unable to fetch desc info for $name"; }

	      foreach my $row (@$res){
		  $description .= $row->[0];
	      }
	      
	      $sthTax->execute($name);
	      my $rfres = $sthTax->fetchall_arrayref;

	      if(! defined $rfres) { die "ERROR unable to fetch tax info for $name"; }
	      foreach my $row (@$rfres){
		  $species   .= $row->[0];
		  $taxString .= $row->[1];
		  $ncbiId    .= $row->[2];
	      }

	      $shortSpecies  = species2shortspecies($species);
	      $domainKingdom = tax2kingdom($taxString . '; ' . $species . ';');
	      $kingdomCounts{$domainKingdom}=0 if not defined $kingdomCounts{$domainKingdom};
	      $kingdomCounts{$domainKingdom}++;
	  }
	  
	  $description  = 'NA' if not defined $description;
	  $species      = 'NA' if not defined $species;
	  $shortSpecies = 'NA' if not defined $shortSpecies;
	  
	  # determine seqLabel
	  my $seqLabel = 'FULL';
	  my ($seed_seq, $overlapExtent) = $msa->nse_overlap($name . "/" . $start . "-" . $end);

	  $seedseq_foundH{$seed_seq}++;

	  if ($overlapExtent > 0.1)                        { $seqLabel = 'SEED'; }
	  if (($bits < $thrcurr) && ($seqLabel ne 'SEED')) { $seqLabel = 'NOT' }

	  # TODO: make printing of threshold line a function
	  # print out threshold line if nec
	  my $outline;
	  if ( $bits < $thrcurr && $thrcurr<=$prv_bits){
	      $outline = "#***********CURRENT THRESHOLD: $thrcurr bits***********#\n";
	      print OUT $outline;
	      print SPC $outline;
	      printf RIN  "%0.2f\tTHRESH\t\.\t.\n", $thrcurr;
	      $printed_thresh=1;
	  }
	  $prv_bits = $bits;

	  if ($evalue > 0.0001 && $prv_evalue <= 0.0001) { 
	      $outline = "#***********E-VALUE OF 1E-4***********#\n";
	      print OUT $outline;
	      print SPC $outline;
	  }
	  
	  if ($evalue > 1 && $prv_evalue <= 1) { 
	      $outline = "#***********E-VALUE OF 1**************#\n";
	      printf OUT $outline;
	      printf SPC $outline;
	  }
	  $prv_evalue = $evalue;

	  printf OUT ("%0.2f\t%0.12s\t%0.6s\t%0.20s\t%10d\t%10d\t%6d\t%6d\t%0.5s\t%0.20s\t%0.70s\n", 
		      $bits, $evalue, $seqLabel, $name, $start, $end, $qstart, $qend, $trunc, $shortSpecies, $description);
	  printf SPC ("%0.2f\t%0.12s\t%0.6s\t%0.20s\t%15d\t%0.20s\t%s\n", 
		      $bits, $evalue, $seqLabel, $name, $ncbiId, $species, $taxString);
	  printf RIN  "%0.2f\t%0.6s\t$domainKingdom\n", $bits, $seqLabel;
	  
      } # closes 'if($tblline =~ m/^\#/) {'
    } # closes 'while($tblline = <TBL>)'
    $rfdbh->disconnect;
    
    if ( not defined $printed_thresh){
	printf OUT "#***********CURRENT THRESHOLD: $thrcurr bits***********#\n";
	printf SPC "#***********CURRENT THRESHOLD: $thrcurr bits***********#\n";
	printf RIN "%0.2f\tTHRESH\t\.\t\.\n", $thrcurr;
    }
    
    foreach my $king ( sort{ $a cmp $b } keys %kingdomCounts) {
	printf RINc  "%d\t$king\n", $kingdomCounts{$king};
    }
    
    close(TBL);
    close(OUT);
    close(SPC);
    close(RIN);
    close(RINc);

   my $RPlot = $config->RPlotScriptPath;
   system("R CMD BATCH --no-save $RPlot") and warn "WARNING: system call for R $RPlot failed. Check binary exists and is executable.\n[R CMD BATCH --no-save $RPlot]\n";
    
    #Complain loudly if seed sequences are missing from the output:
    foreach my $n (keys %seedseq_foundH){
	if ($seedseq_foundH{$n} < 1){
	    printf          "WARNING: SEED sequence $n was not in the OUTPUT!\n";
	    push(@warningsA, "WARNING: SEED sequence $n was not in the OUTPUT!\n");
	}
    }

    # TODO: hook taxinfo, subalign and comparison back up, look at make/orig-files/rfmake.pl for reference,
    # I've deleted all that code here.

    open( WARN, ">warnings" ) or warn "Can't open warnings files\n";
    foreach my $w (@warningsA){
	print WARN $w;
    }
    close(WARN);
} # end of if($do_list) end of LIST mode block
######################################################################
# THRESHOLD mode
else { 
    if($do_X) { 
	if(! defined $famObj->DESC->CUTGA) { die "ERROR with -X we expect GA set in DESC"; }
	$thr = $famObj->DESC->CUTGA;
    }	

    # check for required files
    my $outlistI = "outlist";
    my $file;
    foreach $file ($outlistI) { 
	if(! -s $file) { die "ERROR: required file $file does not exist or is empty\n"; }
    }
    if(! defined $thr) { 
	die "ERROR: threshold is not defined"; 
    }

    setThresholds($famObj, $thr, $outlistO);

    $io->makeAndWriteScores($famObj, "outlist");

    if($do_align) { 
	# make sure we have a valid SCORES file
	# TODO: replace this with FamilyIO
	my $sfetchPath = $config->easelPath . "/esl-sfetch";
	if(! -s "SCORES") { die "ERROR SCORES does not exist to supply to esl-sfetch"; }
	eslSfetch_Cf($sfetchPath, $dbfile, "SCORES", "$$.fa");

        # use cmalign to do the alignment
	# TODO write process_infernal_cmdline_options() to use with both rfsearch and rfmake
	my $options = "";
	if (@cmosA){#covers the '-' options
	    foreach my $opts (@cmosA) {
		$options .= " \-$opts ";
	    }
	}
	if (@cmodA){#covers the '--' options 
	    foreach my $opts (@cmodA) { 
		$options .= " \-\-$opts ";
	    }
	}
        # Run cmalign locally or on farm (autodetermined based on job size, unless -a or -n used)
	my $cmalignPath = $config->infernalPath . "/cmalign";
	cmAlign($cmalignPath, "CM", "$$.fa", "align", "cmalign.out", $options, $famObj->SCORES->numRegions, $famObj->SCORES->nres, ($farm), (! $farm), $dirty);

        # remove temporary fasta file
	if(! $dirty) { unlink "$$.fa"; }
    } # end of if($do_align)
    # VERY IMPORTANT, WRITE OUT DESC FILE!
    $io->writeDESC($famObj->DESC);
}


exit 0;


###########################
# SUBROUTINES

######################################################################
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

sub setThresholds { 
    my ($famObj, $ga, $outlist) = @_;

    my ($tc, $nc, $bits, $line);
    $nc = 0;
    $tc = 999999;

    open(OUTLIST, "$outlist") or die "FATAL: failed to open $outlist\n[$!]";

    while($line = <OUTLIST>){
	if($line !~ m/^\#/) { 
	    # first token is bit score
	    chomp $line;
	    $bits = $line;
	    $bits =~ s/\s+.*$//;
	    
	    if($ga <= $bits && $bits < $tc) { $tc = $bits; }
	    if($ga  > $bits && $bits > $nc) { $nc = $bits; }
	}
    }
    
    if((! defined $nc) || ($nc == 0)) { $nc = "undefined"; }
    print "(GA: $ga, TC: $tc, NC: $nc)\n";

    $famObj->DESC->CUTGA($ga);
    $famObj->DESC->CUTTC($tc);
    $famObj->DESC->CUTNC($nc);

    return;
}

######################################################################
sub tax2kingdom {
    my ($species) = @_;
    my $kingdom;
    #unclassified sequences; metagenomes; ecological metagenomes.
    if ($species=~/^(.+?);\s+(.+?)\.*?;/){
	$kingdom = "$1; $2";
    }
    die "FATAL: failed to parse a kingdom from species string: [$species]" if not defined $kingdom;
    
    return $kingdom;
}

######################################################################
sub truncString2Code {
    my $str = ($_[0]);
    if($str eq "no")    { return 0;  }
    if($str eq "5'")    { return 5;  }
    if($str eq "3'")    { return 3;  }
    if($str eq "5'&3'") { return 53; }
    die "ERROR truncString2Code() invalid input $str";
}


######################################################################
#species2shortspecies: Given a species string eg. "Homo sapiens
#                      (human)" generate a nicely formated short name
#                      with no whitespace eg. "H.sapiens".
sub species2shortspecies {
    my $species = shift;

    my $shortSpecies;
    
    if ($species=~/(.*)\s+sp\./){
	$shortSpecies = $1;
    }
    elsif ($species=~/metagenome/i or $species=~/uncultured/i){
	$species=~s/metagenome/metag\./gi;
	$species=~s/uncultured/uncult\./gi;
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
    }#lots of conditions here. Need else you get some ridiculous species names.
    elsif($species=~/^(\S+)\s+(\S{4,})/ && $species!~/[\/\-\_0-9]/ && $species!~/^[a-z]/ && $species!~/\svirus$/ && $species!~/\svirus\s/ && $species!~/^Plasmid\s/i && $species!~/\splasmid\s/i){
	$shortSpecies = substr($1,0,1) . "." . $2; 
    }
    else {
	$shortSpecies = $species;
    }
    
    $shortSpecies =~ s/\s+/_/g;
    $shortSpecies =~ s/[\'\(\)\:\/]//g;
    $shortSpecies = substr($shortSpecies,0,20) if (length($shortSpecies) > 20);
    
    return $shortSpecies;
}

######################################################################
sub help {
    print STDERR <<EOF;
    
rfmake.pl - process the results of rfsearch.pl. 
  TODO: overhaul rfmake.pl so it only runs in 'threshold' mode, put 'list' mode functionality in rfsearch,
        then rewrite synopsis immediately below:
          - rfmake.pl processes OUTPUT files from rfsearch/cmsearch. In the first round \42-l\42
	    is used to produce a \42list\42 file that contains a tabular summary of the hits,
	    based upon this output, the score distributions in list.pdf \& list.trunc.pdf 
	    and the MCC plot in list_accuracy.dat the CURATOR chooses a threshold with which 
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
