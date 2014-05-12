#!/usr/bin/env perl

# rmfam_scan.pl - 	RNA Motif Family Scan - Searches a database of motif CMs (made using cmpress) 
#                       against a Rfam family SEED stockholm file and populates the RMfam database with the results 
#                       linking families to motifs and vice-versa. Much of this is based on rfsearch.pl
# 			with the difference that is uses cmscan. It has been written in manner as to allow
# 		        future use of cmscan within the Rfam code base.

#################################################################################
# Set modules/packages to use
use warnings;
use strict;
use Cwd;
use Getopt::Long;
use IO::File;
use Data::Printer;
use Carp;
use Data::Dumper;

use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::Family::MSA;
use Bio::Rfam::Infernal;
use Bio::Rfam::Utils;

use Bio::Easel::MSA;
use Bio::Easel::SqFile;
use Bio::Easel::Random;

#################################################################################
# Premilinaries
# - set deafult values that command line options may change
# - process command line options
# - set input/output file names and ensure input files exist
# - process the CM files

my $start_time = time();
my $executable = $0;

# Set the default values that may be changed via command line options

my $local;
my $global;
my $thresh;
my $evalueThresh;
my $help;
my $config = Bio::Rfam::Config->new;  # Set the location of programs and general config
my $no_rev_search = 1;                # TRUE to skip reversed search. No need for reverse as we know the correct strand (given in the SEED)


# The following 3 variables are yet to be used but may be implemented when pasing the cmscan results into the database
my $idf			= 0.9; 				# Value for esl-weight: filter sequences by $idf fraction identity 
my $fractionMotifs	= 0.1;				# Fraction of sequences in a filteres alignment covered by a motif before inclusion
my $minNumberHits	= 2;				# Min number of sequences covered by a motif before inclusion


# database related options, the majority of which are not currently used in rmfam_scan but have been
# included for future use.
my $dbchoice = "rfamseq";       # dbchoice, by default 'rfamseq'
my $dbfile;                     # defined by GetOptions() if -dbfile is enabled
my $dbdir;                      # defined by GetOptions() if -dbdir is enabled
my $dblist;                     # defined by GetOptions() if -dblist is enabled
my $noZ = 0;                    # set to 1 if -noZ used
my $rev_dbfile;                 # defined by GetOptions() if -rdbfile is enabled
my $rev_dbdir;                  # defined by GetOptions() if -rdbdir is enabled
my $Zuser;                      # defined by GetOptions() if -Z is enabled
my $rev_Zuser;                  # defined by GetOptions() if -rZ is enabled
my $dbconfig;


# Get the options from the command line
GetOptions( 	"local"		=> \$local,
		"global"	=> \$global,
	     	"t=s"           => \$thresh,
	     	"e=s"           => \$evalueThresh,
	     	"fm=s"          => \$fractionMotifs,
	     	"min=s"         => \$minNumberHits,
	     	"idf=s"         => \$idf,
	     	"h"             => \$help)
           || die "ERROR, unrecognized option\n";	

# Get the CMdb file name and the Rfam Accession from the command line arguments
my $CMdb = shift;
my $rfam_acc = shift;

# ADD A LINE TO LOG STOUT HERE
# $do_stdout = ($do_quiet) ? 0 : 1;
# open($logFH, ">rfsearch.log") || die "ERROR unable to open rfsearch.log for writing";
# Bio::Rfam::Utils::log_output_rfam_banner($logFH, $executable, "build, calibrate, and search a CM against a database", $do_stdout);


# Determine if help has been specified as command line option
if ( $help ) {
  &help();
  exit(1);
}

# Specify the CMdb directory and seed directory. We must change this to retrieve from config once the 
# database has actually been setup.
my $CM_dir;
my $SEED_dir;
my $CMdb_loc;
my $rfam_seed_loc; 
$CM_dir="/homes/evan/RMfam/RMfam/CMs/";
$SEED_dir="/homes/evan/RMfam/RMfam/Rfam_SEEDS/";
$CMdb_loc=$CM_dir . $CMdb;
$rfam_seed_loc=$SEED_dir . $rfam_acc;


# Determine if files specified as arguments exist
if(not -e $CMdb_loc or not -e $rfam_seed_loc) {    
  print "MISSING the essential CM file or Family Accession!\n";
  &help();
  exit(1);
}     

  #################
  # cmscan step # 
  #################

  my $idx;                         # counter
  my $search_wall_secs        = 0; # wall time (secs) for search
  my $search_cpu_secs         = 0; # CPU time (secs) for all regular (non-reversed) db searches 
  my $search_max_wait_secs    = 0; # max time (secs) a job waited on cluster
  my $search_max_cpu_secs     = 0; # max CPU time (secs) a regular db search job took
  my $search_max_elp_secs     = 0; # max time (secs) elapsed a regular db search job took
  my $rev_search_cpu_secs     = 0; # CPU time (secs) for all reversed db searches
  my $rev_search_max_cpu_secs = 0; # max CPU time (secs) a reversed db search job took
  my $rev_search_max_elp_secs = 0; # max time (secs) elapsed a reversed db search job took
  my $ndbfiles                = 0; # number of db files searched (number of cmsearch calls)
  my $rev_ndbfiles            = 0; # number of reversed db files searched (number of cmsearch calls)
  my $did_search              = 0;

  ################################################################################
  # Database setup; Defining the database which is the simply the family SEED file in this case.
  # This is complete overkill for the purposes of RMfam however it has been included for re-use in the
  # future if cmscan is used with Rfam and to be as analagous to the rfsearch.pl script as possible.
  
  # first, the regular (non-reversed) database:
  $ndbfiles = 0;          # number of sequence files to search
  my @dbfileA  = ();      # array of seq file names for regular search
  if(defined $dbfile) {  # -dbfile used on command line 
    push(@dbfileA, $dbfile);    }
  elsif(defined $dbdir) { # -dbdir used on command line
    push(@dbfileA, glob("$dbdir/*.fa $dbdir/*.fa.gz"));
  }
  elsif(defined $dblist) { # -dblist used on command line
    open(DBLIST, $dblist) || die "ERROR unable to open file $dblist (from -dblist option)"; 
    while(my $file = <DBLIST>) { 
      if($file =~ m/\w/) { 
        chomp $file; 
        push(@dbfileA, $file); 
        if(! -e $file) { die "ERROR file $file listed in $dblist does not exist"; }
      }
    }
    close(DBLIST);
  }
   
  # The following lines are commented out as they pertain to searching a db whilst we wish to
  # search a single SEED against a single CMdb. This currently points to a local SEED but will
  # must be changed in the future.
  #
  #else { # default case: neither -dbfile nor -dbdir used, use database defined in config
  #  for($idx = 0; $idx < $dbconfig->{"nSearchFiles"}; $idx++) { 
  #    $dbfileA[$idx] = $dbconfig->{"searchPathPrefix"} . ($idx+1) . $dbconfig->{"searchPathSuffix"};
  #  }
  #}
  else { $dbfileA[0] = "/homes/evan/RMfam/RMfam/Rfam_SEEDS/$rfam_acc"} 
  
  $ndbfiles = scalar(@dbfileA);

  # setup reversed database to search (this block is analogous to one above for regular (non-reversed) search)
  $rev_ndbfiles = 0;    # number of reversed seq files to search
  my @rev_dbfileA = (); # array of seq file names for reversed search
  my $rev_dbconfig;     # rev db info from config, defined only if $dbconfig already defined and "revMate" exists
  if(! $no_rev_search) { 
    if(defined $rev_dbfile) { # -dbfile used on command line 
       push(@rev_dbfileA, $rev_dbfile);
     }
    elsif(defined $rev_dbdir) { # -dbdir used on command line
     push(@rev_dbfileA, glob("$rev_dbdir/*.fa $rev_dbdir/*.fa.gz"));
    }
    elsif(defined $dbconfig && defined $dbconfig->{"revMate"}) { 
      # default case: neither -rdbfile nor -rdbdir used, use reversed db defined in config if available
      $rev_dbconfig = $config->revseqdbConfig($dbconfig->{"revMate"});
      for($idx = 0; $idx < $rev_dbconfig->{"nSearchFiles"}; $idx++) { 
        $rev_dbfileA[$idx] = $rev_dbconfig->{"searchPathPrefix"} . ($idx+1) . $rev_dbconfig->{"searchPathSuffix"}; 
      }
    }
  }
  $rev_ndbfiles = scalar(@rev_dbfileA);
  if($rev_ndbfiles == 0) { $no_rev_search = 1; }
  # note that if -dbdir, -dbdir or -dbfile used, no reversed searches are done unless -rdbfile or -rdbdir
  #
  # end of database setup
  ##################################################################################

  ##################################################################################
  # Determine cmscan command line options: 
  # Currently -E (E-value threshold) and --ncpu are the only options however this 
  # could incorperate any of the cmscan option as is done with cmsearch in the 
  # rfsearch script.
  
  # Define either a threshold value: either E-value or bit score, only one and it 
  # must always be defined
  my $thr_scanopts;
  if (defined $evalueThresh) { #Must change to check if a bit score threshold has been set as well)
    $thr_scanopts = sprintf("-T %.2f", $evalueThresh);
  }
  else {die "No e-value or bitscore option defined\n"}
   
  # Define the ncpus option for cmscan
  my $ncpus_cmscan; 
  if (! defined $ncpus_cmscan) { $ncpus_cmscan = 4; }
     
  # Use the same reporting threshold for regular and reversed searches
  my $searchopts     = "--cpu $ncpus_cmscan --verbose " . $thr_scanopts;
  my $rev_searchopts = "--cpu $ncpus_cmscan --verbose " . $thr_scanopts;

  $searchopts      =~ s/\s+/ /g; # replace multiple spaces with single spaces
  $searchopts      =~ s/\s+$//;  # remove trailing spaces
  $searchopts      =~ s/^\s+//;  # remove leading spaces
  $rev_searchopts  =~ s/\s+/ /g; # replace multiple spaces with single spaces   
  $rev_searchopts  =~ s/\s+$//;  # remove trailing spaces
  $rev_searchopts  =~ s/^\s+//;  # remove leading spaces
    
  # End of block defining cmsearch options
  ###################################################################################
  
  ##################################################################################
  # Run cmscan via the submit_scan_jobs function which utilises Bio::Rfam::Infernal

  #Bio::Rfam::Infernal::cmscan_wrapper($config,$jobname,$options,$cmPath, $seqfilePath, $outPath, $errPath, $submitExStr, $queue);
  my @jobnameA     = (); # names of jobs for regular searches
  my @tblOA        = (); # names of --tblout files for regular searches
  my @cmsOA        = (); # names of cmsearch output files for regular searches
  my @errOA        = (); # names of error files for regular searches
  my @rev_jobnameA = (); # names of jobs for reversed searches
  my @rev_tblOA    = (); # names of --tblout files for reversed searches
  my @rev_cmsOA    = (); # names of cmsearch output files for reversed searches
  my @rev_errOA    = (); # names of error files for reversed searches
  my $ssopt_str    = ""; 
  my $q_opt        = "";

  submit_cmscan_jobs($config, $ndbfiles, "/homes/evan/RMfam/RMfam/$rfam_acc/",  $searchopts, $CMdb_loc, \@dbfileA, \@jobnameA, \@tblOA, \@cmsOA, \@errOA, $ssopt_str, $q_opt); 

  if($rev_ndbfiles > 0) { 
    submit_cmsearch_jobs($config, $rev_ndbfiles, "s.", $rev_searchopts, $CMdb_loc, \@rev_dbfileA, \@rev_jobnameA, \@rev_tblOA, \@rev_cmsOA, \@rev_errOA, $ssopt_str, $q_opt);
   }

######################################################################################
# The submit_scan_jobs function analagous to the submit_cmsearch_jobs function in rfsearch.pl

sub submit_cmscan_jobs {
  my ($config, $ndbfiles, $prefix, $searchopts, $cmfile, $dbfileAR, $jobnameAR, $tblOAR, $cmsOAR, $errOAR, $ssopt_str, $q_opt) = @_;
  my ($idx, $file_idx, $dbfile);
  
  for($idx = 0; $idx < $ndbfiles; $idx++) { 
    $file_idx = $idx + 1; # off-by-one w.r.t $idx, because database file names are 1..$ndbfiles, not 1..$ndbfiles-1
    $jobnameAR->[$idx] = $prefix . "$$.$file_idx";
    $tblOAR->[$idx]    = $prefix . "$$.$file_idx.tbl";
    $cmsOAR->[$idx]    = $prefix . "$$.$file_idx.cmscan";
    $errOAR->[$idx]    = $prefix . "$$.$file_idx.err";
    Bio::Rfam::Infernal::cmscan_wrapper($config, $jobnameAR->[$idx], "--tblout " . $tblOAR->[$idx] . " " . $searchopts, $cmfile, $dbfileAR->[$idx], $cmsOAR->[$idx], $errOAR->[$idx], $ssopt_str, $q_opt, 0);
  }
}

######################################################################################

sub help {
  print STDERR <<EOF;

# TO BE UPDATED

rmfam_scan.pl: 	Search using motif CMs against a database of sequences or alignments

Usage:      	rmfam_scan.pl [options] <CM> <STOCKHOLM/FASTA>

Options:	    

	Options
        	-h              : show this help
		-v              : verbose - prints lots of largely unimportant information
    	Cmsearch options:
		-t <bits>       : specify cutoff in bits      [DEFAULT is to use curated GA thresholds]
		--local         : perform local mode search   [DEFAULT]
		--global        : perform global mode search

    	RMfam options:
		-f|--fasta      : search database is in fasta format
		For searching alignments:
		-fm  <num>      : fraction of sequences a motif must hit before inclusion [DEFAULT: $fractionMotifs]
		-min <num>      : number   of sequences a motif must hit before inclusion [DEFAULT: $minNumberHits]
		-idf <num>      : filter out seqs w/ fractional ident > num [DEFAULT: $idf]
	
    	Output options:
		-o              : output file
		-e|--embl       : output in EMBL format
		-g|--gff        : output in GFF format                 [DEFAULT for fasta input]
		-a|--aln        : output in annotated Stockholm format [DEFAULT for Stockholm input]
		-n|--net        : output in tabular format for network visualisation
	
    	Miscellaneous options:
		--pid           : restart a job using precomputed cmsearch results
	
      	TODO:
       	 	--sort out a sensible use for outfile
		--use tree weighting on the sum of bit scores metric
		--allow a threshold on the sumBits score
		--add some more caveats to what gets summed for fm <a fuzzy alignment approach - window can be proportional to the specificity of the motif model>
		--record model specificity in the alignment?
		--add support for HMM and-or PWMs
        	--add secondary structure information to the annotated alignment?
		--add an unstranded option (removes --toponly from cmsearch)
		--for fasta files present a better summary. Eg. sumBits (not clans), list motifs, ...
	
		--test unfiltered runs
		--try multiple CMs for each family with eg. base-triples, pseudoknots, ...
		--only report the highest scoring terminator motif (eg. compete "clans")
	
		--add a double-strand option...
		--add a test for concordance with the consensus structure

		--update code for Infernal 1.rc2
		--add support for Elena-s alignment annotation format
EOF
}
