#!/usr/bin/env perl

# rmfam_make.pl - 	Parse the results from the TBL output into a the database 

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
use Bio::Rfam::MotifMatch;
use Bio::Rfam::Infernal;
use Bio::Rfam::Utils;
use RfamLive::ResultSet::MotifMatch;

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
my $config = Bio::Rfam::Config->new;


# The following 3 variables are yet to be used but may be implemented when pasing the cmscan results into the database
my $idf			= 0.9; 				# Value for esl-weight: filter sequences by $idf fraction identity 
my $fractionMotifs	= 0.1;				# Fraction of sequences in a filteres alignment covered by a motif before inclusion
my $minNumberHits	= 2;				# Min number of sequences covered by a motif before inclusion


# database related options, the majority of which are not currently used in rmfamscan but have been
# included for future use 
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
           || die "ERROR, unrecognized option motif";
# Get the Rfam Accession from the command line arguments
my $rfam_acc = shift;


# Determine if help has been specified as command line option
if ( $help ) {
  &help();
  exit(1);
}

my $TBL;
$TBL="/nfs/production/xfam/rfam/MOTIFS/results/$rfam_acc/TBL";


# Determine if file specified as arguments exist
print "TBL = $TBL\n";
if(not -e $TBL) {    
  print "MISSING the essential TBL file\n";
  &help();
  exit(1);
}     

# Parse the TBL file line by line:
open(TBL, "grep -v ^'#' $TBL | sort -nrk 15 | ") || croak "FATAL: could not open pipe for reading $TBL\n[$!]";

  my $tblline;
  
  while ($tblline = <TBL>) {
    my ($bits, $evalue, $motif_id, $rfamseq_id, $CMstart, $CMend, $qStart, $qEnd, $strand, $trunc) = processTblOutLineCM($tblline);
  
    my ($rfamseq_acc, $rfamseq_start, $rfamseq_stop);
   
    # Split up the Rfamseq into acc, start and stop
    if ($rfamseq_id   =~ /(\S+.\d)\/(\d+)-(\d+)/) {
      $rfamseq_acc     = $1;
      $rfamseq_start   = $2;
      $rfamseq_stop    = $3;
    }   
    else { print "Could not understand the sequence identifier $rfamseq_id\n
                  Is it in the correct format?\n";
    }

    # Create the MotifMatch Object which contains the results from TBLOUT
    my $motifMatchObj = Bio::Rfam::MotifMatch->new;
 
    $motifMatchObj->RFAM_ACC($rfam_acc);
    
    $motifMatchObj->RFAMSEQ_ACC($rfamseq_acc);
    $motifMatchObj->RFAMSEQ_START($rfamseq_start);
    $motifMatchObj->RFAMSEQ_STOP($rfamseq_stop);
    
    $motifMatchObj->QUERY_START($qStart);
    $motifMatchObj->QUERY_STOP($qEnd);

    $motifMatchObj->MOTIF_START($CMstart);
    $motifMatchObj->MOTIF_STOP($CMend);
    
    $motifMatchObj->E_VALUE($evalue);
    $motifMatchObj->BIT_SCORE($bits);
  
    # Create the DB object
    my $rfamdb = $config->rfamlive;

    # Determine the Motif Accession from the Motif ID
    my $motif_result = $rfamdb->resultset('Motif')->find({ motif_id => $motif_id} );
    my $motif_acc = $motif_result->motif_acc;
    $motifMatchObj->MOTIF_ACC($motif_acc);    
  
    #Need to put a transaction around this block
    my $guard = $rfamdb->txn_scope_guard;
   
    # Insert the values from the MotifMatch Object into the Motif Match Table using ResultSet 
    $rfamdb->resultset('MotifMatch')->find_or_createFromMotifMatchObj( $motifMatchObj );

    $guard->commit;
}

  
  

#####################################################################################
sub processTblOutLineCM {
  my ($tblline) = @_;

  ## example TBLOUT line:
  ##
  ## target name         accession query name           accession mdl mdl from   mdl to seq from   seq to strand trunc pass   gc  bias  score   E-value inc description of target
  ## ------------------- --------- -------------------- --------- --- -------- -------- -------- -------- ------ ----- ---- ---- ----- ------ --------- --- ---------------------
  ## twist_up             -         X01556.1/3-118       -          cm        1       21       29       49      +    no    1 0.57   0.0   27.2   1.9e-06 !   -

  my @tblA = split(/\s+/, $tblline);
  my ($motif_name, $qName, $CMstart, $CMend, $qStart, $qEnd, $strand, $trunc, $bits, $evalue) = ($tblA[0], $tblA[2], $tblA[5], $tblA[6], $tblA[7], $tblA[8], $tblA[9], $tblA[10], $tblA[14], $tblA[15]);

  return ($bits, $evalue, $motif_name, $qName, $CMstart, $CMend, $qStart, $qEnd, $strand, $trunc);
}



######################################################################################

sub help {
  print STDERR <<EOF;

motif_make.pl: 	Search using motif CMs against a database of sequences or alignments

Usage:      	motif_make.pl [options] <RF_ACC>

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
