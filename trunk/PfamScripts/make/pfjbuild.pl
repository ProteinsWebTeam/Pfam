#!/usr/local/bin/perl

use strict;
use warnings;
use Getopt::Long;
use IO::File;
use Sys::Hostname;
use Cwd;
use File::Copy;
use File::Rsync;
use File::stat;
use Data::UUID;

use Bio::Pfam::Config;
use Bio::Pfam::AlignPfam;
use Bio::Pfam::HMM::HMMResultsIO;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::SeqFetch;

main(@ARGV) unless caller();

sub main {

#-------------------------------------------------------------------------------
# Get the Pfam Config and check all is well.
  my $config = Bio::Pfam::Config->new;

  unless ($config) {
    die
"Failed to obtain a Pfam Config object, check that the environment variable PFAM_CONFIG is set and the file is there!\n";
  }
  unless ( $config->location eq 'WTSI' or $config->location eq 'JFRC' ) {
    warn "Unkown location.....things will probably break\n";
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
    $incDomT, $incDomE, $help,  $noOverlap, $local, $acc, $copy, $gzip
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
    "copy"      => \$copy,
    "acc=s"     => \$acc,
    "gzip"      => \$gzip,
    "help"      => \$help
  ) or help();


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
      warn "\n***** You need to specify a valid fasta file or a UniProt accession! *****\n\n";
      $help = 1;
  }

  if ( !$incE and !$incDomT and !$incT ) {
    warn
"\n***** Neither e-value or bit score threshold specified, setting to 0.001 *****\n\n";
    $incE = 0.001;
  }

  if ($incE) {
    if ( $incDomT or $incT ) {
      warn
"\n***** You can not specify a bit score threshold and an E-value threshold *****\n\n";
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
    warn
"\n****** You need to specifiy both a sequence and domain bits score threshold *****\n\n";
    $help = 1;

  }

  help() if ($help);


  #$check is true if the user requested the checkpoint files
  #$c contains the name of the checkpoint files
  my $c = "$$.chkpnt"; #Use $c later for writing PFAMOUT file
  if ($check) {
      $optCmds{'--chkali'} = $check;
      $optCmds{'--chkhmm'} = $check;
      $c = $check;
  }
  else {
    warn "\n***** Checkpoint has been turned off. *****\n\n"; #We'll still going to run with check pointing, but we won't copy checkpointing files back
    $optCmds{'--chkali'} = $c;
    $optCmds{'--chkhmm'} = $c;
  }
  


  unless ($seqDB) {
    $seqDB = $config->pfamseqLoc . "/pfamseq";
  }

  unless ($noOverlap) {
    if ( $seqDB !~ m|/pfamseq$| ) {
      warn
"$seqDB does not look like it is pfamseq.  Switching off overlap check\n";
      $noOverlap = 1;
    }
  }

  help() if ($help);

#-------------------------------------------------------------------------------
#/software/pfam/src/hmmer-3.0b2/bin/jackhmmer -A JALIGN -o output fa /nfs/pfam_nfs/pfam/pfamseq/pfamseq
#run with default of   --notextw

  #Run two other options by default

  $optCmds{'-A'}        = 'JALIGN';
  $optCmds{'-o'}        = 'JOUT';
  $optCmds{'--notextw'} = '';
  $optCmds{'--cpu'}     = '1';

  if ($local) {
    runJackhmmer( $config, \%optCmds, $fasta, $seqDB );
    
    #Check for overlaps....
    unless ( $noOverlap and -s "JALIGN" ) {
      checkOverlap($pfamDB);
    }

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

    unless($check) {
	unlink glob("$c*");
    }
    if($gzip) {
	system("gzip JALIGN") and die "Failed to run gzip on JALIGN, $!";
	system("gzip JOUT") and die "Failed to run gzip on JOUT, $!";
    }
    unlink("JOUT.final");


  }
  else {
    farmJackhmmer( $config, \%optCmds, $fasta, $seqDB, $noOverlap, $gzip, $check, $c, $copy );
  }

}

sub checkOverlap {
  my ($pfamDB) = @_;
  open( A, "JALIGN" )  or die "Could not open JALIGN:[$!]\n";
  open( T, ">$$.tmp" ) or die;
  while (<A>) {
    if ( $_ !~ /^#/ and $_ =~ /\S+/ ) {
      print T $_;
    }
  }
  close(A);
  close(T);
  open( T, "$$.tmp" ) or die;

  my $align = Bio::Pfam::AlignPfam->new;

  $align->read_Pfam( \*T );

  my %regions;
  foreach my $seq ( $align->each_seq ) {
    push @{ $regions{ $seq->id } },
      {
      from      => $seq->start,
      to        => $seq->end,
      family    => ("NEW"),
      ali       => 'JALIGN',
      family_id => ("NEW")
      };
  }

  my %overlaps;
  $pfamDB->getOverlapingFullPfamRegions( \%regions, \%overlaps );
  $pfamDB->getOverlapingSeedPfamRegions( \%regions, \%overlaps );

  my $numOverlaps = 0;
  my %seen;

  #Now print out any overlaps that should not be ignored
  my $LOG;
  open( $LOG, ">overlap" ) or die "Can't open overlap file\n";

  foreach my $seqAcc ( keys %overlaps ) {
    foreach
      my $region ( sort { $a->{from} <=> $b->{from} } @{ $overlaps{$seqAcc} } )
    {
      foreach my $overRegion ( @{ $region->{overlap} } ) {
        my $line =
            "Sequence [" 
          . $seqAcc
          . "] overlap "
          . $region->{family_id} . " "
          . $region->{family} . "/"
          . $region->{from} . "-"
          . $region->{to} . " "
          . $region->{ali}
          . " with "
          . $overRegion->{family_id} . " "
          . $overRegion->{family} . "/"
          . $overRegion->{from} . "-"
          . $overRegion->{to} . " "
          . $overRegion->{ali} . "\n";

        next if ( $seen{$line} );
        $seen{$line}++;
        $numOverlaps++;
        print STDERR $line;
        print $LOG $line if $LOG;
      }
    }
  }
  close $LOG if ($LOG);
  close T;
  unlink("$$.tmp");
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

  system($cmd) and die "Failed to run jackhmmer:[$!]\n";
}

sub farmJackhmmer {
  my ( $config, $optCmdsRef, $fasta, $seqDB, $noOverlap, $gzip, $check, $c,  $copyFiles ) = @_;

  unless ( -e $fasta ) {
    die "FATAL: Could not find fasta file, $fasta\n";
  }

  unless ( -e $seqDB ) {
    die "FATAL: Could not find sequence database file: $seqDB\n";
  }

  #We are going to use some sort of farm!
  my $farmConfig = $config->farm;
  unless ($farmConfig) {
    die "Failed to get a farm configuration file\n";
  }

  #Need does it look like the sequence database in on a farm disk?

  #mkdir on the lustre disk and copy our files there
  my $ug   = new Data::UUID;
  my $uuid = $ug->to_string( $ug->create() );
  my $user = $ENV{USER};

  #Make the director
  mkdir( $farmConfig->{lsf}->{scratch} . "/$user/$uuid" )
    or die "Failed to make a directory on the farm users space, "
    . $farmConfig->{lsf}->{scratch}
    . "/$user/$uuid: [$!]";

  copy( $fasta, $farmConfig->{lsf}->{scratch} . "/$user/$uuid/$fasta" )
    or die "Failed to copy SEED to scratch space:[$!]";

  $fasta = $farmConfig->{lsf}->{scratch} . "/$user/$uuid/$fasta";

  unless ( $seqDB =~ m|^/nfs/pfam_nfs| ) {
    warn
"Going to copy sequence database to scratch, this may slow things down a bit!\n";
    copy( $seqDB, $farmConfig->{lsf}->{scratch} . "/$user/$uuid/$seqDB" )
      or die "Failed to copy SEED to scratch space:[$!]";
    $seqDB = $farmConfig->{lsf}->{scratch} . "/$user/$uuid/$seqDB";
  }

#Okay, things should be in the right place for working on.
#Build up the command we want to run.  We want to run this same script, but with the local option.

  my $cmd = "pfjbuild.pl";

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
  $fh->open( "| bsub -q "
      . $farmConfig->{lsf}->{queue} . " -o "
      . $farmConfig->{lsf}->{scratch}
      . "/$user/$uuid/$$.log -Jjackhmmer$$" );
  if($copyFiles){
    $fh->print( "cd " . $farmConfig->{lsf}->{scratch} . "/$user/$uuid \n" );
  }else{
    $fh->print( "cd $pwd \n" );
  }
  #Execute the command we have built up.
  $fh->print("$cmd\n");

  #Write a PFAMOUT style file
  my $jout_file = $farmConfig->{lsf}->{scratch} . "/$user/$uuid/JOUT";
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


  #Bring back all of the files.
  if($copyFiles){
      if($gzip) {
	  $fh->print("/usr/bin/scp JALIGN.gz $phost:$pwd/JALIGN.gz\n");	 
	  $fh->print("/usr/bin/scp JOUT.gz $phost:$pwd/JOUT.gz\n");
      }
      else {
	  $fh->print("/usr/bin/scp JALIGN $phost:$pwd/JALIGN\n");
	  $fh->print("/usr/bin/scp JOUT $phost:$pwd/JOUT\n");
      }
   
    $fh->print("/usr/bin/scp overlap $phost:$pwd/overlap\n") unless ($noOverlap);
    $fh->print("/usr/bin/scp align $phost:$pwd/align\n");
    $fh->print("/usr/bin/scp $check\* $phost:$pwd/. \n") if($check);

    $fh->print("/usr/bin/scp HMM $phost:$pwd/HMM\n");
    $fh->print("/usr/bin/scp PFAMOUT $phost:$pwd/PFAMOUT\n");
    $fh->print("/usr/bin/scp DESC $phost:$pwd/DESC\n");
  }
  
  #Now clean up after ourselves on the farm
  $fh->print( "rm -fr " . $farmConfig->{lsf}->{scratch} . "/$user/$uuid \n" ) if($copyFiles);
  $fh->close();

}


sub writePFAMOUT {
    #This subroutine is virtually identical to the pfjbuild_pfamout.pl script.  
    #Should really put it in a module.

    my ($check, $c) = @_;  
                           
    #Write results of final iteration to file
    my ($header, $header_complete, $flag);
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
	    }
	    elsif(/^Description\:/) {
		$header .= "$_\n";
		$header_complete = 1;
	    }
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

usage: $0 -fa <fasta file> -db <sequence db>  or $0 -acc <accession> -db <sequence db>
 
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
-noOverlap    : Script 
-local        : Run script on local machine
-gzip         : Gzips the JOUT and JALIGN files, this option is useful if you are running
                a large number of pfjbuilds and are conscious of disk space
-help         : Prints out this help message

Options that you can not control 
i)   Options controlling scoring system in iteration one.
ii)  Options controlling significance thresholds for reporting.
iii) Turning off HMMER3 acceleration heuristics
iv)  Expert parameters for HMM building.


EOF

  exit;

}
