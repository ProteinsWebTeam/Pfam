#!/usr/local/bin/perl

use strict;
use warnings;
use Getopt::Long;

use Bio::Pfam::Config;
use Bio::Pfam::AlignPfam;
use Bio::Pfam::HMM::HMMResultsIO;
main( @ARGV ) unless caller(  );

sub main {
  my $config = Bio::Pfam::Config->new;
  
  unless($config){
    die "Failed to obtain a Pfam Config object, check that the environment variable PFAM_CONFIG is set and the file is there!\n";
  }
  unless ($config->location eq 'WTSI' or $config->location eq 'JFRC'){
    warn "Unkown location.....things will probably break\n"; 
  }
  
  my ($fname, $hand, $local, $nobuild, $nosplit);


  &GetOptions( "hand"    => \$hand,
               "local"   => \$local,
               "nobuild" => \$nobuild,
               "nosplit" => \$nosplit );
               
  # Do we have a SEED file and can we build an HMM for it.
  unless($nobuild){
    my @buildOpts;
    unless(-s 'SEED'){
      die "Could not locate the SEED file:[$!]\n";
    }
    open( SEED, "SEED" ) or die "FATAL: can't open SEED\n";
    open( SNEW, ">SEED.$$" ) or die "FATAL: can't write to SEED.$$\n";
  
    my $aln = Bio::Pfam::AlignPfam->new;
    eval{
      $aln->read_stockholm( \*SEED );
    };
  
    #Problem reading the alignment
    if($@){
      warn "SEED alignment not in stockholm format, trying seleex/Pfam format.\n";
    }
    #Seed if it looks like a SEED in Pfam/Mul format
    eval{
      $aln->read_Pfam( \*SEED );
    };
  
    if($@){
      die "Failed to parse the SEED alignment\n";
    }
    
    #Now reformat the SEED alignment to make all - characters .  
    my $newaln = $aln->allgaps_columns_removed();
    $newaln->map_chars( '-', '.' );
    $newaln->write_stockholm( \*SNEW );
    if($newaln->match_states_string){
      push(@buildOpts, "--hand");
    }
    close SEED;
    close SNEW;

    rename( "SEED", "SEED.old.$$" ) or die "FATAL: can't rename SEED\n";
    rename( "SEED.$$", "SEED" ) or die "FATAL: can't rename SEED.$$\n";

    my $buildline = join( ' ', @buildOpts );
    unless( -d $config->hmmer3bin){
      die "Could not find the HMMER3 bin directory,". $config->hmmer3bin ."\n";
    }
    
    system($config->hmmer3bin."/hmmbuild -o hmmbuild.log ".$buildline." HMM SEED") and die "Error building hmm!\n";
    unless(-s "HMM"){
      die "Failed to run HMM build. Although it seems to have run successfully, the HMM has no size or is absent\n"; 
    }
  }else{
    # Skip hmmbuild if no build
    unless( -s "HMM" ){
      die "Could not find the HMM file:[$!]\n";
    } 
  }


  # Grab all of the file ownership

  # Now build and search
  my $searchOptions = "--seqE 1000 --seqZ ".$config->dbsize;
    
  my $cmd;
  my $HMMResultsIO = Bio::Pfam::HMM::HMMResultsIO->new;  
  if($nosplit){
    $cmd =$config->hmmer3bin."/hmmsearch $searchOptions HMM ".$config->pfamseqLoc."/pfamseq > OUTPUT";
  }
  
  if($local){
    if($nosplit){
      system($cmd) 
        and die "Failed to run hmmbuild [ $cmd ] due to [$!]";
      #Parse the Results
      $HMMResultsIO->convertHMMSearch( "OUTPUT" );
    }else{
      #TODO
      #Work on a split version 
    }
  }else{
    #We are going to use some sort of farm! 
    my $farmConfig = $config->farm;
    unless($farmConfig){
      die "Failed to get a farm configuration file\n"; 
    }
    if($farmConfig->{sge}){
      if($nosplit){
        system("qsub -N pfamHmmSearch -j y -o /dev/null -b y -cwd -V \'$cmd\'") and die "Failed to submit job to SGE,qsub -N pfamHmmSearch -j -o /dev/null -b y -cwd -V \'$cmd\' \n";  
      }else{
        #TODO split
        print STDERR "TODO: nothing run:\n"
      } 
    }elsif($farmConfig->{lsf}){
      if($nosplit){
        #system("qsub -N pfamHmmSearch -j y -o /dev/null -b y -cwd -V \'$cmd\'") and die "Failed to submit job to SGE,qsub -N pfamHmmSearch -j -o /dev/null -b y -cwd -V \'$cmd\' \n";  
      }else{
        #TODO split
        print STDERR "TODO: nothing run:\n"
      } 
    }else{
      die "Unknown farm set-up\n"; 
    }
  }

  print STDERR "Finished\n";
}
