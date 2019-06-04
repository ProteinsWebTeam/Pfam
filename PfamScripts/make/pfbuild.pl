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
use Data::UUID;
use POSIX qw(ceil);

use Bio::Pfam::Config;
use Bio::Pfam::AlignPfam;
use Bio::Pfam::HMM::HMMResultsIO;
use Bio::Pfam::FamilyIO;
use Data::Dumper;

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
#Deal with the command line options

  my (
    $fname, $hand,       $local,      $nobuild, $split,
    $help,  $evalCut,    $dbsize,     $max,     $bFilt,
    $null2, $f1,         $f2,         $f3,      $ibm,
    $ism,   $withpfmake, $makeEvalue, $removeBadEd, $db,      $cpu,
    $copy,  $pfamseq_local, $memory_gb, $modelLength
  );

  &GetOptions(
    "help"       => \$help,
    "hand"       => \$hand,
    "ignoreBM"   => \$ibm,
    "ignoreSM"   => \$ism,
    "local"      => \$local,
    "copy"       => \$copy,
    "nobuild"    => \$nobuild,
    "split"      => \$split,
    "cpu=i"      => \$cpu,
    "E=s"        => \$evalCut,
    "Z=s"        => \$dbsize,
    "max"        => \$max,
    "nobias"     => \$bFilt,
    'nonull2'    => \$null2,
    'F1=s'       => \$f1,
    'F2=s'       => \$f2,
    'F3=s'       => \$f3,
    'withpfmake' => \$withpfmake,
    'makeEval=s' => \$makeEvalue,
    'removeBadEd'=> \$removeBadEd,     
    'pfamseq=s'  => \$pfamseq_local,
    'M=i'        => \$memory_gb,
    'db=s'       => \$db
  )
    or die "Unknown option, try running -help for more infortmation.\n";

  help() if ($help);
  if ( $hand and $nobuild ) {
    warn "\n***** Can not specfiy -hand and -build together *****\n\n";
  }

  if($removeBadEd and !$withpfmake) {
    die "Cannot specify -removeBadEd option without specifying -withpfmake option.\n";
  }

  unless ( $config->location eq 'WTSI' or $config->location eq 'JFRC' or $config->location eq 'EBI' ) { 
    unless($local) {
      warn "Location is not EBI, switching on -local option\n";
      $local=1;
    }   
  }

  if ($pfamseq_local) {
    unless ($local) {
      die "You can only use the -pfamseq option with the the -local option\n";
    }
    unless ( -s $pfamseq_local ) {
      die "[$pfamseq_local] does not exist\n";
    }
  }

  if ($local) {
    $split = 0;
  }


  $db = "pfamseq" unless ($db);
  if($cpu) {
  }
  elsif($db eq 'mgnify') {
    $cpu=16;
  }
  else {
    $cpu = 4;
  }

  #Check db and dbsize
  my $db_location;
  if ( $db eq "pfamseq" ) {
    if ( $dbsize and $dbsize ne $config->dbsize ) {
      warn
      "\n***** Using effective database size [$dbsize] that is different to pfamseq ["
      . $config->dbsize
      . "] *****\n\n";
    }
    else {
      $dbsize = $config->dbsize;
    }

    if ($pfamseq_local) {
      $db_location = $pfamseq_local;
    }
    else {
      if ( $config->location eq 'WTSI' ) {
        $db_location = $config->pfamseqLustreLoc . "/$db";
      }
      else {
        $db_location = $config->pfamseqLoc . "/$db";
      }
    }
  }
  elsif ( $db eq "uniprot" )
  {
    if ( $dbsize and $dbsize ne $config->uniprot_dbsize )
    {
      warn "\n***** Using effective database size [$dbsize] that is different to uniprot sequence db [" . $config->uniprot_dbsize . "] *****\n\n";
    }
    else
    {
      $dbsize = $config->uniprot_dbsize;
    }
    if ( $config->location eq 'WTSI' )
    {
      $db_location = $config->pfamseqLustreLoc . "/$db";
    }
    else
    {
      $db_location = $config->uniprotLoc . "/$db";
    }
  }
  elsif ( $db eq "ncbi" ) {
    if ( $dbsize and $dbsize ne $config->ncbi_dbsize ) {
      warn
      "\n***** Using effective database size [$dbsize] that is different to ncbi sequence db ["
      . $config->ncbi_dbsize
      . "] *****\n\n";
    }
    else {
      $dbsize = $config->ncbi_dbsize;
    }
    if ( $config->location eq 'WTSI' ) {
      $db_location = $config->pfamseqLustreLoc . "/$db";
    }
    else {
      $db_location = $config->ncbiLoc . "/$db";
    }
  }
  elsif( $db eq "mgnify") {
    if ( $dbsize and $dbsize ne $config->{mgnify}->{dbsize} ) {   
      warn "\n***** Using effective database size [$dbsize] that is different to mgnify sequence db [" . $config->{mgnify}->{dbsize} . "] *****\n\n";
    }   
    else {   
      $dbsize = $config->{mgnify}->{dbsize};
    }   
    $db_location = $config->{mgnify}->{location_zipped};
  }
  elsif ( $db eq "shuffled" ) {
    if ( $dbsize and $dbsize ne $config->dbsize ) {
      warn
      "\n***** Using effective database size [$dbsize] that is different to shuffled ["
      . $config->shuffle_dbsize
      . "] *****\n\n";
    }
    else {
      $dbsize = $config->shuffled_dbsize;
    }
    if ( $config->location eq 'WTSI' ) {
      $db_location = $config->pfamseqLustreLoc . "/$db";
    }
    else {
      $db_location = $config->shuffledLoc . "/$db";
    }
  }
  else {
    die
    "db must be either 'pfamseq', 'shuffled', 'ncbi' or 'mgnify', you specified [$db]\n";
  }
  unless ( int($dbsize) == $dbsize and $dbsize > 0 ) {
    die "dbsize ($dbsize) must be an integer greater than 1\n";
  }


  #If outside of EBI, check the sequence database is up to date
  unless($config->location eq 'EBI') {
    check_dbsize($db, $config);
  }

#-------------------------------------------------------------------------------
#Read in the DESC file.  This is now required!
  my $io = Bio::Pfam::FamilyIO->new;

  unless ( -s 'DESC' ) {
    warn
    "We now require a DESC file before running $0.\n Writing dummy DESC file\n";
    $io->writeEmptyDESC;
  }

  my $descObj = $io->parseDESC("DESC");

#-------------------------------------------------------------------------------
  unless ($local) {
    foreach my $f ( @{ $config->mandatoryFamilyFiles } ) {
      if ( -e $f ) {
        my $info = stat($f);

        #See if we, the user, own the file
        if ( $< != $info->uid ) {

          #move it sideways then copy back!
          rename( $f, "$f.old" );
          if ( $f eq "SEED" or $f eq "DESC" ) {
            copy( "$f.old", $f );
          }
          elsif ( ( $f eq "scores" or $f eq "ALIGN" ) and !$withpfmake ) {
            copy( "$f.old", $f );
          }
        }
      }
    }
  }

#-------------------------------------------------------------------------------
# If we are to run HMM build check the SEED and build options.

  # Do we have a SEED file and can we build an HMM for it.
  unless ($nobuild) {
    my %buildOpts;
    unless ( -s 'SEED' ) {
      die "Could not locate the SEED file:[$!]\n";
    }
    open( SEED, "SEED" )     or die "FATAL: can't open SEED\n";
    open( SNEW, ">SEED.$$" ) or die "FATAL: can't write to SEED.$$\n";
    open( SNEWS, ">SEED.$$.selex" )
      or die "FATAL: can't write to SEED.$$.selex\n";
    my $aln = Bio::Pfam::AlignPfam->new;

    #eval{
    #  $aln->read_stockholm( \*SEED );
    #};

    #Problem reading the alignment
    #if($@){
    #  warn "SEED alignment not in stockholm format, trying selex/Pfam format.\n";
    #}
    #Seed if it looks like a SEED in Pfam/Mul format
    eval { $aln->read_Pfam( \*SEED ); };

    if ($@) {
      die "Failed to parse the SEED alignment\n";
    }

    #Now reformat the SEED alignment to make all - characters .
    my $newaln = $aln->allgaps_columns_removed();
    $newaln->map_chars( '-', '.' );
    $newaln->write_stockholm( \*SNEW );
    $newaln->write_Pfam( \*SNEWS );

    if ( $newaln->match_states_string ) {
      $buildOpts{"--hand"} = 0;
      unless ($hand) {
        warn "Found #=RF line, switching on --hand options\n";
      }
    }
    else {
      if ($hand) {
        die "You switched on -hand but found no #=RF line in the SEED.\n";
      }
    }
    close SEED;
    close SNEW;
    close SNEWS;

    rename( "SEED",    "SEED.old.$$" ) or die "FATAL: can't rename SEED\n";
    rename( "SEED.$$", "SEED" )        or die "FATAL: can't rename SEED.$$\n";

    $buildOpts{"-o"} = '/dev/null';

    unless ($ibm) {
      my $line = $descObj->BM;
      if ( $line =~ /hmmbuild (.*) HMM/ ) {
        while ( $line =~ /(-{1,2}\w+)\s+([A-Z0-9\.]){0,1}/g ) {
          my $optFlag  = $1;
          my $optParam = $2 if ($2);

          unless ( defined( $buildOpts{$optFlag} ) ) {
            if ($optParam) {
              $buildOpts{$optFlag} = $optParam;
            }
            else {
              $buildOpts{$optFlag} = 0;
            }
          }

        }
      }
      elsif ( $line =~ /hmmbuild\s+HMM/ ) {
        ;
      }
      else {
        die "Did not recognise BM line " . $descObj->BM . "\n";
      }
    }
    my $buildline = "hmmbuild ";
    foreach my $opt ( keys %buildOpts ) {
      if ( $buildOpts{$opt} ) {
        $buildline .= " $opt " . $buildOpts{$opt};
      }
      else {
        $buildline .= " $opt";
      }
    }

    $buildline .= " HMM SEED";

    $descObj->BM($buildline);

    #Now run hmmbuild
    system( $config->hmmer3bin . "/" . $buildline )
      and die "Error building hmm!\n";
    unless ( -s "HMM" ) {
      die
      "Failed to run HMM build. Although it seems to have run successfully, the HMM has no size or is absent\n";
    }

    rename( "SEED.$$.selex", "SEED" ) or die "FATAL: can't rename SEED.$$\n";

  }
  else {

    # Skip hmmbuild if no build
    unless ( -s "HMM" ) {
      die "Could not find the HMM file:[$!]\n";
    }
  }


  #If running at Sanger/EBI, need to estimate how much memory will be needed on farm, to do this we will need the HMM length
  if ( $config->location eq 'WTSI' or $config->location eq 'EBI' ) { 
    die "No HMM in cwd" unless(-s "HMM");
    open(HMM, "HMM") or die "Couldn't open fh to HMM, $!";
    while(<HMM>) {
      if(/^LENG\s+(\d+)/) {
        $modelLength=$1;
        last;
      }
    }   
    close HMM;
  }   
#-------------------------------------------------------------------------------
# Now build up the hmmsearch options

  # Now build and search
  my %searchOptions;

  $searchOptions{"--cpu"} = $cpu;

  # E-value cut off
  unless ($evalCut) {
    $evalCut = 1000;
  }
  unless ( $evalCut > 0 ) {
    die "You can not specifiy a E-value cutoff less than 0\n";
  }
  $searchOptions{"-E"} = $evalCut;

  # Db size
  $searchOptions{'-Z'} = $dbsize;

  # Turn off heuristic filtering
  if ($max) {
    if ($local) {
      warn "\n***** This will take a while to run *****\n\n";
    }
    $searchOptions{"--max"} = 0;
  }

  if ($bFilt) {
    $searchOptions{"--nobias"} = 0;
  }

  if ($null2) {
    warn "\n***** Switching off biased sequence correction scores *****\n\n";
    $searchOptions{"--nonull2"} = 0;
  }

  if ($f1) {
    warn "\n***** Setting the multi sequence viterbi threshold to $f1 *****\n";
    $searchOptions{"--F1"} = $f1;
  }
  if ($f2) {
    warn "\n***** Setting viterbi threshold to $f2 *****\n";
    $searchOptions{"--F2"} = $f2;
  }
  if ($f3) {
    warn "\n***** Setting the Forward score filter to $f3 *****\n";
    $searchOptions{"--F3"} = $f3;
  }

  unless ($ism) {
    my $line = $descObj->SM;
    if ( $line =~ /hmmsearch (.*) HMM/ ) {
      while ( $line =~ /(-{1,2}\w+)\s+([A-Z0-9\.]){0,1}/g ) {
        my $optFlag  = $1;
        my $optParam = $2 if ($2);

        unless ( defined( $searchOptions{$optFlag} ) ) {
          if ($optParam) {
            $searchOptions{$optFlag} = $optParam;
          }
          else {
            $searchOptions{$optFlag} = 0;
          }
        }

      }
    }
    elsif ( $line =~ /hmmsearch\s+HMM/ ) {
      ;
    }
    else {
      die "Did not reecognise SM line " . $descObj->SM . "\n";
    }
  }

  my $searchOptions = 'hmmsearch';
  foreach my $opt ( keys %searchOptions ) {
    if ( $searchOptions{$opt} ) {
      $searchOptions .= " $opt " . $searchOptions{$opt};
    }
    else {
      $searchOptions .= " $opt";
    }
  }
  $searchOptions .= " HMM";

#-------------------------------------------------------------------------------
  #

  my $cmd;
  my $HMMResultsIO = Bio::Pfam::HMM::HMMResultsIO->new;
  unless ($split) {
    $cmd = $config->hmmer3bin . "/" . $searchOptions . " $db_location > OUTPUT";
    $descObj->SM( $searchOptions . " $db" );
  }

  #Okay if we get here, this is a great chance of success!
  rename( "DESC", "DESC.b4.pfbuild" )
    or die "Could not move DESC to DESC.b4.pfbuild";
  $io->writeDESC($descObj);

  unlink('PFAMOUT');
  unlink('OUTPUT');

  if ($local) {
    system($cmd) and die "Failed to run hmmbuild [ $cmd ] due to [$!]";

    #Parse the Results
    $HMMResultsIO->convertHMMSearch("OUTPUT");

    #run pfmake if we need to
    if ($withpfmake) {
      my $pfmake_db;
      if ( $db eq "pfamseq" and $config->location eq "WTSI" ) { 
        $pfmake_db = $config->pfamseqLoc . "/$db";
      }   
      else {
        if($db eq 'mgnify') {
          $pfmake_db = $config->{mgnify}->{location} ;
        }   
        else {
          $pfmake_db = $db_location;
        }   
      }    
      my $pfmake_cmd.="pfmake -d $pfmake_db ";

      if($removeBadEd) {
        $pfmake_cmd.= "-removeBadEd ";
      }

      if ($makeEvalue) {
        $pfmake_cmd .= "-e $makeEvalue ";
      }
      elsif(!-e "DESC" ) {
        $pfmake_cmd .= "-e 0.01 ";
      }
      #else DESC must exist so it will use the GA in the DESC file

      system("$pfmake_cmd\n") and die "Failed to run pfmake\n";

    }
  }
  else {

    #Get the current working directory and hostname
    my $phost = hostname;
    my $pwd   = getcwd;

    #We are going to use some sort of farm!
    my $farmConfig = $config->farm;
    unless ($farmConfig) {
      die "Failed to get a farm configuration file\n";
    }

    if ( $farmConfig->{sge} ) {
      unless ($split) {
        system("qsub -N pfamHmmSearch -j y -o /dev/null -b y -cwd -V \'$cmd\'")
          and die
        "Failed to submit job to SGE,qsub -N pfamHmmSearch -j -o /dev/null -b y -cwd -V \'$cmd\' \n";
      }
      else {

        #TODO split
        print STDERR "TODO: nothing run:\n";
      }
    }
    elsif ( $farmConfig->{lsf} ) {

      #First check that pfamseq is most up to date and if not, copy files to farm
      my $rsyncObj = File::Rsync->new(
        {
          compress     => 1,
          recursive    => 1,
          rsh          => '/usr/bin/ssh',
          'rsync-path' => '/usr/bin/rsync',
          checksum     => 1
        }
      );

      #Check the pfamseq is up-to-date
      #TODO - put this back after we have arleady done this.
      #$rsyncObj->exec( { src  => $config->pfamseqLoc()."/pfamseq",
      #                   dest => $config->pfamseqFarmLoc()."/pfamseq" });
      my $ug   = new Data::UUID;
      my $uuid = $ug->to_string( $ug->create() );
      my $user = $ENV{USER};
      if ($copy) {
        mkdir( $farmConfig->{lsf}->{scratch} . "/$user/$uuid" )
          or die "Failed to make a directory on the farm users space, "
        . $farmConfig->{lsf}->{scratch}
        . "/$user/$uuid: [$!]";
        copy( "SEED", $farmConfig->{lsf}->{scratch} . "/$user/$uuid/SEED" )
          or die "Failed to copy SEED to scratch space:[$!]";
        copy( "HMM", $farmConfig->{lsf}->{scratch} . "/$user/$uuid/HMM" )
          or die "Failed to copy HMM to scratch space:[$!]";
        copy( "DESC", $farmConfig->{lsf}->{scratch} . "/$user/$uuid/DESC" )
          or die "Failed to copy DESC to scratch space:[$!]";
      }

      unless ($split) {
        my $fh = IO::File->new();

        unless($memory_gb) {
          #Going to estimate how much memory will be needed using this equation:
          #(Model length L * 40000 (longest sequence) * 48 (number of bytes in the dp) * number of cpus) /1,000,000 
          $memory_gb = ceil(($modelLength * 40000 * 48 * $cpu)/1000000000); 

          #If estimated memory > 8Gb, reduce number of threads to see if memory can be reduced to less than 8Gb
          unless($db eq 'mgnify') { 
            while($memory_gb >=8) {
              $cpu--;
              if($cpu == 0) {
                die "Cannot run pfbuild as estimated amount of memory required to run this pfbuild is more than 8Gb\n";
              } 
              $memory_gb = ceil(($modelLength * 40000 * 48 * $cpu)/1000000000);
              print STDERR "If $cpu cpus used, $memory_gb Gb memory required\n";
              if($memory_gb < 8) {
                print STDERR "Reducing number of cpus to $cpu to reduce the estimated amount of memory required to below 8Gb\n";
              }
            }
          }
          print STDERR "Reserving $memory_gb Gb of memory on farm\n";
        }

        my $memory_mb=$memory_gb*1000;
        my $memory_kb=$memory_mb*1000;


        if( $config->location eq "WTSI") {
          $fh->open( "| bsub -q "
            . $farmConfig->{lsf}->{queue}
            . " -n $cpu -R \"span[hosts=1] select[mem>$memory_mb] rusage[mem=$memory_mb]\" -M $memory_kb -o /tmp/$$.log -Jhmmsearch$$ -G pfam-grp"
          );
        }
        elsif( $config->location eq "EBI") {

          $fh->open( "| bsub -q "
            . $farmConfig->{lsf}->{queue}
            . " -n $cpu -R \"rusage[mem=$memory_mb]\" -M $memory_mb -o pfbuild.log -Jhmmsearch$$"
          );
        }
        else {
          die "Config is not WTSI or EBI, do not know what to do";
        }

        if ($copy) {
          $fh->print(
            "cd " . $farmConfig->{lsf}->{scratch} . "/$user/$uuid \n" )
          if ($copy);
        }
        else {
          $fh->print( "cd " . $pwd . " \n" );
        }
        $fh->print("$cmd\n");

        # now need to do the equivalent of convertHMMsearch method in$HMMResultsIO;
        $fh->print("pfbuild_farm_post_search.pl\n");

        #And finally, run pfmake if we need to
        if ($withpfmake) {
          my $pfmake_db;

          if ( $db eq "pfamseq" and $config->location eq "WTSI" ) { 
            $pfmake_db = $config->pfamseqLoc . "/$db";
          }   
          else {
            if($db eq 'mgnify') {
              $pfmake_db = $config->{mgnify}->{location} ;
            }   
            else {
              $pfmake_db = $db_location;
            }   
          }   
          my $pfmake_cmd.="pfmake -d $pfmake_db ";

          if($removeBadEd) {
            $pfmake_cmd.= "-removeBadEd ";
          }

          if ($makeEvalue) {
            $pfmake_cmd .= "-e $makeEvalue ";
          }
          elsif (!-e "$pwd/DESC" ) {
            $pfmake_cmd .= "-e 0.01 ";
          }
          #else DESC must exist so it will use the GA in the DESC file

          $fh->print("$pfmake_cmd\n");
        }

        #Now bring back all of the files
        if ($copy) {
          $fh->print("/usr/bin/scp -p HMM $phost:$pwd/HMM\n");
          $fh->print("/usr/bin/scp -p PFAMOUT $phost:$pwd/PFAMOUT\n");
          $fh->print("/usr/bin/scp -p OUTPUT $phost:$pwd/OUTPUT\n");

          if ($withpfmake) {

            #Copy all of the files back associated with the alignment.
            $fh->print("/usr/bin/scp -p DESC $phost:$pwd/DESC\n");
            $fh->print("/usr/bin/scp -p scores $phost:$pwd/scores\n");
            $fh->print("/usr/bin/scp -p ALIGN $phost:$pwd/ALIGN\n");
          }

          #Now clean up after ourselves on the farm
          $fh->print(
            "rm -fr " . $farmConfig->{lsf}->{scratch} . "/$user/$uuid \n" );
        }
        $fh->close();
      }
      else {

        #TODO split
        print STDERR "TODO: nothing run:\n";
      }
    }
    else {
      die "Unknown farm set-up\n";
    }
  }
}

sub help {

  print <<EOF;

Command line options for controlling $0 
-------------------------------------------------------------------------------

  -help       : prints this help messeage

If you do not understand any of the options below, just run without any options.

Options that influence hmmbuild:

  -nobuild    : If you have already built the HMM and do not want to build 
              : it again.
  -hand       : When there is an #=RF line in the SEED alignment, this option 
              : should be switched on.  The #=RF line dictates which residues are 
              : matche states. In Pfam, this is often used to mask out nested 
              : domanins.
  -ignoreBM   : Ignore the BM line present in the DESC file. Otherwise the BM
              : line will be supplimented to your BM options.  

  Both of these options can not be supplied together;

  *** There are more specialised options to come ***

Options that influence hmmsearch:

  -db <x>     : Specify which database to search against (choose pfamseq||uniprot||mgnify||ncbi, default is pfamseq)

  General wrapping options:
  -local      : Run the hmmsearch on the local machine rather than submitting 
              : to a compute farm. Note, the farm configuration is used by the 
              : Pfam configuration file.
  -pfamseq <p>: Give the location of pfamseq on the command line
                This option is only for use with -local
  -copy       : Copy the files in the familyto a place that is visible to the farm.            
  -split      : Run the hmmsearch against the split version of pfamseq (Not yet implemented).
  -ignoreSM   : Ignore the SM line present in the DESC file. Otherwise the SM
              : line will be supplimented to your SM options.            

  Note, if -local is specified then split is switched off.

  Options controlling significance thresholds for reporting:
  -E <x>      : E-value cutoff for reporting sequences  [default 1000].
  -Z <x>      : Effective size of the sequence database for E-value calculation
              : This should be an unsigned integer.  [default is size of pfamseq
              : grabbed from the Pfam config].

  Note both -E and -Z only effect the sequence parameters and not the --domE and 
  --domZ parameters in HMMER3.  

  Options controlling acceleration heuristics:
  -max        : Turn all heuristic filters off (increase sensitivity). This is 
              : not recommended for genral Pfam building as it will run very slowly, 
              : but useful for ensuring that no sequences are rejected by the 
              : filtering.
  -nobias     : Turn off composition bias filter. **Do not check in a family with this option**

Options for gurus:
  -nonull2    : turn off biased sequence composition corrections to scores    
  -F1 <x>     : Stage 1 (MSV) threshold: promote hits w/ P <= F1  [0.02]
  -F2 <x>     : Stage 2 (Vit) threshold: promote hits w/ P <= F2  [1e-3]
  -F3 <x>     : Stage 3 (Fwd) threshold: promote hits w/ P <= F3  [1e-5]

  Note, option -max is incompatible with option(s) -F1,-F2,-F3

And Finally:

  -withpfmake : run pfmake after the search.  If there is a DESC file present, 
              : then it will use the threshold present in the file. Otherwise,
              : it will use a default threshold of 10e-2.
  -makeEval   : Will run pfmake with the specified evalue cut-off   
  -removeBadEd: Will run pfmake with -removeBadEd option (need to use this option in conjuction with -withpfmake option)
  -M <int>    : Amount of memory in Gb to request (this option is only for EBI farm)

EOF

  exit(1);

}

sub check_dbsize {
  my ($db, $config) = @_;

  my $svn_url;
  if($db eq 'pfamseq') {
    $svn_url = $config->{pfamseq}->{svn_dbsize};
  }
  elsif($db eq 'uniprot') {
    $svn_url = $config->{uniprot}->{svn_dbsize};  
  }
  else {
    die "Search database must be pfamseq or uniprot if searching outside EBI\n";
  }
  unless($svn_url) {
    die "Could not locate svn url for $db in the pfam config\n";
  }


  my $svn_dbsize;
  open(F, "svn cat $svn_url |") or die "Error running 'svn $svn_url', $!";
  while(<F>) {
    if(/^(\d+)$/) {
      $svn_dbsize=$1;
    }   
    else {
      die "Unrecognised format of database size in svn: $_";
    }   
  }
  close F;

  unless($config->{$db}->{dbsize} eq $svn_dbsize) {
    die "The database size for $db in the pfam config is [".$config->{$db}->{dbsize}."], but the current size of $db is [$svn_dbsize].\nPlease update your $db sequence database, and then update the dbsize in the pfam config\n";
  }
}
