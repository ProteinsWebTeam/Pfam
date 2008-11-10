#!/software/bin/perl

use strict;
use warnings;
use Getopt::Long;

use Bio::Pfam::AlignPfam;

main( @ARGV ) unless caller(  );

sub main {
$ENV{PATH} = "/Users/rdf/Work/HMMER3/PreAlpha/hmmer-3.0.snap20081101/src:$ENV{PATH}";
my ($fname, $hand, $nolsf, $nobuild, $nosplit);


&GetOptions( "hand"    => \$hand,
             "nolsf"   => \$nolsf,
             "nobuild" => \$nobuild );

#Developement stuff
$nosplit = 1;
$nolsf   = 1;
my $dbsize = 5000000;
my $pfamseq = '/Users/rdf/Work/HMMER3/Data/23.0/pfamseq';

# Do we have a SEED file
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

  eval{
    $aln->read_Pfam( \*SEED );
  };
  
  if($@){
    die "Failed to parse the SEED alignment\n";
  }
  
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

  system("hmmbuild $buildline HMM SEED");
  
}

# Set the location of pfamseq
#my $pfamseq;

# Skip hmmbuild if no build

  # If there is an HMM already present, grab the build line.

  # Now set any additional parameters

# Check the SEED format
# Reformat gap characters
# Do we have an RF line


# Grab all of the file ownership

# Now build and search

my $options = "-E 1000  -Z $dbsize";
if($nolsf){
  if($nosplit){
     system("hmmsearch $options HMM $pfamseq > PFAMOUT");
  }
}
}
