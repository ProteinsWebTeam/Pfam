#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use IO::File;
use Sys::Hostname;
use Cwd;
use File::Copy;
use File::Rsync;
use File::stat;


use Bio::Pfam::Config;
use Bio::Pfam::AlignPfam;
use Bio::Pfam::HMM::HMMResultsIO;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::SeqFetch;
use Bio::Pfam::PfamQC;
use Bio::Pfam::CompositionalBias;

main(@ARGV) unless caller();

sub main {

#-------------------------------------------------------------------------------
# Get the Pfam Config and check all is well.
  my $config = Bio::Pfam::Config->new;

  unless ($config) {
    die
    "Failed to obtain a Pfam Config object, check that the environment variable PFAM_CONFIG is set and the file is there!\n";
  }
  unless ( -d $config->hmmer3bin ) {
    die "Could not find the HMMER3 bin directory," . $config->hmmer3bin . "\n";
  }

#-------------------------------------------------------------------------------

  my $connect = $config->pfamlive;

  my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{$connect} );

#-------------------------------------------------------------------------------
#Deal with the command line options
  my (
    $noIts,   $fasta,   $seqDB, $check, $incT,      $incE,
    $incDomT, $incDomE, $help,  $noOverlap, $local, $acc, $gzip, $memory
  );

  Getopt::Long::Configure('no_ignore_case');
  &GetOptions(
    "N=i"       => \$noIts,
    "fa=s"      => \$fasta,
    "db=s"      => \$seqDB,
    "chk=s"     => \$check,
    "e=s"       => \$incE,
    "t=s"       => \$incDomT,
    "T=s"       => \$incT,
    "noOverlap" => \$noOverlap,
    "local"     => \$local,
    "acc=s"     => \$acc,
    "gzip"      => \$gzip,
    "M=i"       => \$memory,	      
    "help"      => \$help
  ) or help();

 unless($config->location eq 'EBI') {
   unless($noOverlap) {
     warn "The overlap check in this script will not work outside of EBI. If you want to run this script outside of EBI, please use the -noOverlap option\n";
     exit;
   }
 }

  my %optCmds;
  unless ($noIts) {
    $noIts = 5;
  }    

  $optCmds{'-N'} = $noIts;

  die "Can't specify both accession and fasta file on the command line\n" if($fasta and $acc);

  if($acc) {
    my %seq;
    push(@{ $seq{$acc} }, { whole => 1 });
    open(FASTA, ">$acc.fasta") or die "Could not open $acc.fasta for writing, $!\n";
    my $seq_fetch = Bio::Pfam::SeqFetch::fetchSeqs(\%seq,$config->pfamseqLoc."/pfamseq", \*FASTA);
    unless($seq_fetch) {
      print STDERR "Couldn't find $acc in pfamseq, going to pfetch the sequence\n";
      open(PFETCH, "pfetch $acc |") or die "Couldn't open fh to pfetch $acc $!\n";
      while(<PFETCH>) {
        if(/no match/) {
          die "Couldn't find $acc using pfetch\n";
        }
        else {
          print FASTA $_;
        }
      }
      close PFETCH;
    }
    close FASTA;
    $fasta = "$acc.fasta";
  }

  unless($fasta and -s $fasta) {
    warn "\n***** You need to specify a valid fasta file or a UniProt accession! *****\n";
    $help = 1;
  }

  if ( !$incE and !$incDomT and !$incT ) {
    warn "\n***** Neither e-value or bit score threshold specified, setting to 0.001 *****\n";
    $incE = 0.001;
  }

  if ($incE) {
    if ( $incDomT or $incT ) {
      warn"\n***** You can not specify a bit score threshold and an E-value threshold *****\n";
      $help = 1;
    }
    else {
      $incDomE = $incE;
    }
    $optCmds{'--incE'}    = $incE;
    $optCmds{'--incdomE'} = $incDomE;
  }

  if ($incDomT) {

    #if only -t has been specified.
    $incT = $incDomT unless ($incT);
    $optCmds{'--incT'}    = $incT;
    $optCmds{'--incdomT'} = $incDomT

  }

  if ( $incT and !$incDomT ) {
    warn "\n****** You need to specifiy both a sequence and domain bits score threshold *****\n";
    $help = 1;

  }

  help() if ($help);

  $memory = 2 unless($memory); #Set default memory to 2Gb 
  if($memory > 20) {
    die "Memory must not be > 20 (unit is Gb)\n";
  }

  #$check is true if the user requested the checkpoint files
  #$c contains the name of the checkpoint files
  #Use $c later for writing PFAMOUT file
  my $c;
  if ($check) {
    $optCmds{'--chkali'} = $check;
    $optCmds{'--chkhmm'} = $check;
    $c = $check;
  }
  else {
    warn "\n***** Checkpoint has been turned off. *****\n"; #We'll still going to run with check pointing, but will delete files later
    $c="chkpnt";
    $optCmds{'--chkali'} = $c;
    $optCmds{'--chkhmm'} = $c;  
  }


  unless ($seqDB) {
    if($config->location eq 'WTSI') {
      $seqDB = $config->pfamseqLustreLoc."/pfamseq";
    }
    else {
      $seqDB = $config->pfamseqLoc . "/pfamseq";
    }
  }


  unless ($noOverlap) {
    if( $seqDB =~ m|/pfamseq$|  ) {
      warn "\n***** Overlap check has been switched on. Overlaps will NOT be filtered according to the config file.\n";

    }
    else {
      warn "$seqDB does not look like it is pfamseq.  Switching off overlap check\n";
      $noOverlap = 1;
    }
  }

  help() if ($help);

#-------------------------------------------------------------------------------
#/software/pfam/src/hmmer-3.0b2/bin/jackhmmer -A JALIGN -o output fa /nfs/pfam_nfs/pfam/pfamseq/pfamseq
#run with default of   --notextw


  $optCmds{'-A'}        = 'JALIGN';
  $optCmds{'-o'}        = 'JOUT';
  $optCmds{'--notextw'} = '';
  $optCmds{'--cpu'}     = '1';

  if ($local) {
    runJackhmmer( $config, \%optCmds, $fasta, $seqDB );

    #Write PFAMOUT style file
    writePFAMOUT($check, $c);


    #Make pseudo ALIGN type file
    open(JALIGN, "JALIGN") or die "Couldn't open file JALIGN $!";
    open(ALIGN, ">align") or die "Couldn't open file 'align' for writing $!";

    while(<JALIGN>) {
      next if(/^\#/ or /^\s*$/ or m|//|);
      print ALIGN $_;
    }
    close JALIGN;
    close ALIGN;

    #Make pseudo ALIGN files for checkpoint alignments
    if($check) {
      for(my $i=1; $i<=$noIts; $i++) {
        my $a = "$check-$i";
        my $aln_file = "$a.sto";
        next unless(-s $aln_file);
        open(ALN, $aln_file) or die "Couldn't open $aln_file, $!";
        open(A, ">$a.align") or die "Couldn't open file '$a.align' for writing $!";

        while(<ALN>) {
          next if(/^\#/ or /^\s*$/ or m|//|);
          print A $_;
        }
        close ALIGN;
        close A;
      }
    }

    #Check for overlaps....
    unless ( $noOverlap ) {
      checkOverlap($pfamDB, "align");
      if($check) {  #Check for overlaps in checkpoint alignments
        for(my $i=1; $i<=$noIts; $i++) {
          my $aln_file = "$check-$i.align";
          next unless(-s $aln_file);
          checkOverlap($pfamDB, $aln_file);
        }
      }
    }

    unless($check) {
      unlink glob("$c*");
    }

    if($gzip) {
      system("gzip JALIGN") and die "Failed to run gzip on JALIGN, $!";
      system("gzip JOUT") and die "Failed to run gzip on JOUT, $!";
    }
    unlink("JOUT.final");

    my $prop_bias = Bio::Pfam::CompositionalBias::calculate_compositional_bias("align");
    open(BIAS, ">prop_bias") or die "Couldn't open fh to prop_bias, $!";
    print BIAS "$prop_bias\n";
    close BIAS;


  }
  else {
    farmJackhmmer( $config, \%optCmds, $fasta, $seqDB, $noOverlap, $gzip, $check, $c, $memory );
  }

}

sub checkOverlap {
  my ($pfamDB, $aln) = @_;

  my (%regions);

  open( A, $aln )  or die "Could not open $aln:[$!]\n";
  while (<A>) {
    next if(/^#/ or /\/\//);
    if(/(\S+)\/(\d+)-(\d+)/) { #Alignments contain alignment co-ordinates
      my ($acc, $st, $en) = ($1, $2, $3);
      if($acc =~ /(\S+)\.\d+/) {
        $acc=$1;
      }

      push(@{ $regions{ $acc } }, 
        {
          ali_from  => $st,   
          ali_to    => $en,
          family    => 'NEW',
          ali       => 'JALIGN',
          family_id => 'NEW' });
    }
  }
  close(A);

  my $ignore_ref;
  my ($numOverlaps, $overlapArray) = &Bio::Pfam::PfamQC::findOverlapsDb(\%regions, $ignore_ref, $pfamDB, "", "", "", "");

  #Now print out any overlaps that should not be ignored
  my $LOG;
  my $overlap_file;
  if($aln eq "align") {
    $overlap_file = "overlap";
  }
  else {
    if($aln =~ /(\S+)\.align/) {
      $overlap_file = "$1.overlap";
    }
    else {
      $overlap_file = "$aln.overlap";
    }
  }
  open( $LOG, ">$overlap_file" ) or die "Can't open overlap file $overlap_file, $!\n";
  print $LOG @{$overlapArray};
  close $LOG;
}

sub runJackhmmer {
  my ( $config, $optionalCmds, $fasta, $seqDB) = @_;

  unless ( -e $fasta ) {
    die "FATAL: Could not find fasta file, $fasta\n";
  }

  unless ( -e $seqDB ) {
    die "FATAL: Could not find sequence database file: $seqDB\n";
  }

  my $cmd = $config->hmmer3bin() . "/jackhmmer ";
  while ( my ( $opt, $param ) = each %$optionalCmds ) {
    $cmd .= "$opt $param ";
  }

  $cmd .= "$fasta $seqDB";
  #print STDERR "Running this command: |$cmd|\n";
  system($cmd) and die "Failed to run jackhmmer:[$!]\n";
}

sub farmJackhmmer {
  my ( $config, $optCmdsRef, $fasta, $seqDB, $noOverlap, $gzip, $check, $c, $memory ) = @_;

  unless ( -e $fasta ) {
    die "FATAL: Could not find fasta file, $fasta\n";
  }

  unless ( -e $seqDB ) {
    die "FATAL: Could not find sequence database file: $seqDB\n";
  }

  my $memory_mb = $memory * 1000;
  my $memory_kb = $memory_mb * 1000;

  #We are going to use some sort of farm!
  my $farmConfig = $config->farm;
  unless ($farmConfig) {
    die "Failed to get a farm configuration file\n";
  }


#Okay, things should be in the right place for working on.
#Build up the command we want to run.  We want to run this same script, but with the local option.

  my $cmd = $0;

  if ( $$optCmdsRef{'--incT'} ) {
    $cmd .= " -T " . $$optCmdsRef{'--incT'};
  }

  if ( $$optCmdsRef{'--incdomT'} ) {
    $cmd .= " -t " . $$optCmdsRef{'--incdomT'};
  }

  if ( $$optCmdsRef{'--chkhmm'} ) {
    $cmd .= " -chk " . $$optCmdsRef{'--chkhmm'};
  }

  if ( $$optCmdsRef{'--incE'} ) {
    $cmd .= " -e " . $$optCmdsRef{'--incE'};
  }

  if($$optCmdsRef{'-N'}){  
    $cmd .= " -N ". $$optCmdsRef{'-N'};
  }

  if ($noOverlap) {
    $cmd .= " -noOverlap";
  }

  $cmd .= " -local -fa $fasta -db $seqDB";

  #Get the current working directory and hostname
  my $phost = hostname;
  my $pwd   = getcwd;

  #useful for debugging
  #print STDERR "Going to run |$cmd| on the farm\n";

  #Now submit the command!
  my $fh = IO::File->new();

  if($config->location eq 'WTSI') { #Sanger farm requires the group to be specified in bsub commands
    $fh->open( "| bsub -q "
      . $farmConfig->{lsf}->{queue} . " -o $$.log -Jjackhmmer$$ -R \"select[mem>$memory_mb] rusage[mem=$memory_mb]\" -M $memory_kb -G pfam-grp" );
  }
  elsif($config->location eq 'EBI') { # EBI memory requirement is specified in Mb
    $fh->open( "| bsub -q " . $farmConfig->{lsf}->{queue} . 
      " -o $$.log" .
      " -Jjackhmmer$$ " .
      " -R \"select[mem>$memory_mb] rusage[mem=$memory_mb]\" " . 
      " -M $memory_mb" ); 
  }
  else {
    $fh->open( "| bsub -q "
      . $farmConfig->{lsf}->{queue} . " -o $$.log -Jjackhmmer$$ -R \"select[mem>$memory_mb] rusage[mem=$memory_mb]\" -M $memory_kb" );
  }

  #Execute the command we have built up.
  $fh->print( "cd $pwd \n" );
  $fh->print("$cmd\n");

  #Write a PFAMOUT style file
  $fh->print("pfjbuild_pfamout.pl JOUT $c\n");
  $fh->print("grep \"^[A-Za-z0-9]\" JALIGN > align \n"); #Create an ALIGN file

  if($gzip) {
    $fh->print("gzip JALIGN\n");
    $fh->print("gzip JOUT\n");
  }  

  $fh->print("rm -fr JOUT.final\n");
  unless($check) {
    $fh->print("rm -fr $c* \n");
  }
  $fh->close();

}


sub writePFAMOUT {
  #This subroutine is virtually identical to the pfjbuild_pfamout.pl script.  
  #Should really put it in a module.

  my ($check, $c) = @_;  

  #Write results of final iteration to file
  my ($header, $header_complete, $flag, $itNum);
  my $outfile = "JOUT.final";
  open(JOUT, "JOUT") or die "Couldn't open JOUT $!";
  while(<JOUT>) {
    #Store header info
    if(!$header_complete) {
      if(/^\#/) {
        $header .= $_;
      }
      elsif(/^Query\:/) {
        $header .= "\n$_";
        $header_complete=1;
        $itNum++;
      }

    }
    elsif(/^Description\:/) { #Not all files will have a description line
      $header .= "$_\n";
    }
    #Print header and results for this iteration, overwriting any previous iterations
    elsif(/^Scores for complete sequences/) {
      open(OUT, ">$outfile") or die "Couldn't open $outfile for writing $!";
      print OUT $header;
      print OUT $_;
      $flag=1;
    }
    elsif($flag) {
      print OUT $_;
    }
  }
  close OUT;

  #Create PFAMOUT style result file
  my $hmmRes = Bio::Pfam::HMM::HMMResultsIO->new;
  my $result =  $hmmRes->parseHMMER3($outfile);
  $hmmRes->writePFAMOUT($result);

  #Create DESC if there isn't one already
  unless(-s 'DESC'){
    my $io = Bio::Pfam::FamilyIO->new;
    $io->writeEmptyDESC;
  }

  my @list = glob("*.hmm");


  my $max=0;
  foreach my $file (@list) {
    if($file =~ /$c\-(\d+)\.hmm/) {
      $max = $1 if($1 > $max);
    }
  }

  my $hmm_file = $c . "-" . $max . ".hmm";
  copy($hmm_file, "HMM") or die "Couldn't copy $hmm_file to HMM $!";    

  unless($check) {
    unlink glob("$c*");
  }

}

sub help {

  print <<EOF;

usage: $0 -fa <fasta file> or $0 -acc <accession>

The alignment in written into the file JALIGN and the list of hits into JOUT.
A PFAMOUT file is produced for the hits in the final iteration.
An 'align' file is written which contains the alignment in the JALIGN file without 
the posterior probabilities.

You can control jackhmmer with the following options

-db <dbname>  : The sequence database that you want search against.  Must be fully qualified path. 
                Default is pfamseq.
-N  <#>       : The number of iterations that you want performed (Integer). Default is 5.
-chk <string> : Whether you want check point files write after each iteration.  String is the root 
              : name for iteration.ali and iteraterion.hmms.

Either
-e            : Inclusion e-value.  This applied to both the sequence and the domain.
or
-T            : Inclusion sequence bit score.  If you specify this, then you must also supply a 
                -t option.
-t            : Inclusion domain bit score.  If -T is not supplied and -t is, then -T is set to -t. 
                i.e. Domain and sequence have the same inclusion threshold.

Script Options
-fa <fa_file> : The name of the fasta file that you want to run Jackhmmer on.
-acc <acc>    : Accession of protein you want to run Jackhmmer on
-noOverlap    : Do not check for overlaps
-local        : Run script on local machine
-gzip         : Gzips the JOUT and JALIGN files, this option is useful if you are running
                a large number of pfjbuilds and are conscious of disk space
-M <int>      : Amount of memory in Gb to request on farm (default 2 Gb)
-help         : Prints out this help message

Options that you can not control 
i)   Options controlling scoring system in iteration one.
ii)  Options controlling significance thresholds for reporting.
iii) Turning off HMMER3 acceleration heuristics
iv)  Expert parameters for HMM building.


EOF

  exit;

}
