#! /usr/bin/env perl

# Software to find matches between Pfam families.
#
# Basically the method compares two HMM output files and finds the
# number of common regions between the two. Two regions are counted as
# common if one of their midpoints lies within the other.
#
# To use the software an input file must be provided. An example is
# shown below:
#
# O00050 PF06465  448     500     2.5e-31
# O00050 PF07453  19      52      1.8e+03
# O00050 PF07572  89      159     9.8e+02
# O00053 PF00012  37      642     0.0
# O00053 PF00416  507     576     3.9e+02
# O00053 PF00630  167     264     2.6e+02
# O00053 PF00929  37      180     8.8e+02
#
#
# The columns are as follows:
#  column 1 protein identifier
#  column 2 pfam family accession
#  column 3 start of match
#  column 4 end of match
#  cloumn 5 E-value of match

$| = 1;
use strict;
use Cwd;
use Bio::SCOOP::Region;
use Bio::SCOOP::RegionSet;
use Getopt::Long;
use Log::Log4perl qw(:easy);

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my $config = Bio::Pfam::Config->new;

my $pfamDBAdmin =
  Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbhAdmin = $pfamDBAdmin->getSchema->storage->dbh;

my $tempDir = getcwd;

$logger->debug("Getting significant regions");
$dbhAdmin->do(
      "SELECT auto_pfamseq, auto_pfamA seq_start, seq_end, domain_evalue_score "
    . "INTO OUTFILE '/tmp/sigReg$$.txt' FROM pfamA_reg_full_significant" );

$logger->debug(
  "Getting insignificant regions with an evalue less than or equal to 1");
$dbhAdmin->do(
      "SELECT auto_pfamseq, auto_pfamA seq_start, seq_end, domain_evalue_score "
    . "INTO OUTFILE '/tmp/insigReg$$.txt' FROM pfamA_reg_full_insignificant WHERE domain_evalue_score<=1"
);

foreach my $file ( "insigReg$$.txt", "sigReg$$.txt" ) {
  system( "scp " . $config->pfamliveAdmin->{host} . ":/tmp/$file $tempDir/$file" );
}

#Now sort the regions
system(
  "cat $tempDir/sigReg$$.txt $tempDir/insigReg$$.txt | sort > $tempDir/allReg.txt")
  and $logger->logdie("Failed to sort the regions:[$!]");

my $out;
open( $out, ">$tempDir/scoopRes.4upload.txt" )
  or $logger->logdie("Failed to open file $tempDir/scoopRes.4upload.txt:[$!]");

runScoop( "$tempDir/allReg.txt", $out, $tempDir, $logger );

close($out);

$pfamDBAdmin->getSchema->resultset('Pfama2pfamaScoopResults')->delete;

system( "scp $tempDir/scoopRes.4upload.txt "
    . $config->pfamliveAdmin->{host}
    . ":/tmp/scoopRes.4upload.txt" )
  and $logger->logdie("Failed to scp results file to instance");
$dbhAdmin->do(
"LOAD DATA INFILE '/tmp/scoopRes.4upload.txt' INTO TABLE pfamA2pfamA_scoop_results"
);

sub runScoop {
  my ( $file, $fh, $tempDir, $logger ) = @_;

  $logger->info("Loading up regions");
  my $total_region = 0;
  my %family_total;    # Counts matches per family
  my %uber_matrix;

  open( FH, $file ) or die "Cannot open $file";
  my $old_id;
  my ( $set, $new_set );
  foreach my $line (<FH>) {
    chomp $line;
    my @line = split( /\s+/, $line );
    my $id   = $line[0];

    # Make new region object
    my $region = new Bio::SCOOP::Region(
      'id'          => $id,
      'start'       => $line[2],
      'end'         => $line[3],
      'family'      => $line[1],
      'evalue'      => $line[4],
      'significant' => $line[5]
    );

    if ( $id ne $old_id and $old_id ) {
      # Now add old set to uber_matrix
      add_set_to_matrix( $set, \%family_total, \%uber_matrix );
      
      # Make new set
      $set = new Bio::SCOOP::RegionSet($id);
      $set->add($region);
      $total_region++;
    }
    elsif ( $id eq $old_id ) {
      $set->add($region);
      $total_region++;
    }
    elsif ( $id ne $old_id and !$old_id ) {
      $set = new Bio::SCOOP::RegionSet($id);
      $set->add($region);
      $total_region++;

    }
    $old_id = $id;
  }
  $logger->info("Finished processing regions");

  # Now do final processing
  $logger->info( "Total regions:$total_region");

  # Variables used to calculate the average score per family
  my %family_score_total;
  my %family_score_denominator;

  # Loop over every pair of families in uber matrix and calculate score
  open( FH, ">$tempDir/old_scores" ) or die "cannot write $tempDir/old_scores:[$!]";
  foreach my $x ( sort keys %uber_matrix ) {
    my %hash = %{ $uber_matrix{$x} };
    foreach my $y ( sort keys %hash ) {
      my $expected = 1 + $family_total{$x} * $family_total{$y} / $total_region;
      my $observed = $uber_matrix{$x}{$y};
      my $test     = 0;
      my $score;
      if ( !$test ) {
        $score = $observed / $expected;
      }
      else {
        $score = $uber_matrix{$x}{$y};
      }
      print FH "$score $x $y $family_total{$y} Both $observed\n";

      # Store for each family the total score and the number of entries.
      $family_score_total{$x} += $score;
      $family_score_total{$y} += $score;
      $family_score_denominator{$x}++;
      $family_score_denominator{$y}++;
    }
  }

  # Print out average score data
  foreach my $x ( keys %family_score_total ) {

    # Method d
    my $average_score =
      $family_score_total{$x} / ( 100 + $family_score_denominator{$x} );
    print FH
"# Average score for $x is $average_score denominator is $family_score_denominator{$x}\n";
  }
  print FH "# TOTAL REGIONS $total_region\n";
  close FH;

  $logger->info("Second pass over Uber matrix\n");
  # Loop over every pair of families in uber matrix and calculate score
  foreach my $x ( sort keys %uber_matrix ) {
    foreach my $y ( sort keys %{ $uber_matrix{$x} } ) {
      my $average_score_x =
        $family_score_total{$x} / ( 100 + $family_score_denominator{$x} );
      my $average_score_y =
        $family_score_total{$y} / ( 100 + $family_score_denominator{$y} );
      my $max_score;
      if ( $average_score_x > $average_score_y ) {
        $max_score = $average_score_x;
      }
      else {
        $max_score = $average_score_y;
      }

      # The below line is current best method
      my $expected =
        1 + $max_score * $family_total{$x} * $family_total{$y} / $total_region;
      my $observed = $uber_matrix{$x}{$y};
      my $test     = 0;
      my $score;

      $score = $observed / ($expected);

      print $fh "$x\t$y\t$score\n";

    }
  }

}

# OK now have all regions loaded up
# Loop over each protein and populate uber matrix.

# This routine takes a RegionSet and does comparison and adds data to uber_matrix
sub add_set_to_matrix {
  my ( $set, $family_total, $uber_matrix ) = @_;
  my @regions = $set->each();
  my $n       = @regions;

  #print STDERR "Looking at protein $id with $n regions\n";
  # Now loop over diagonal half matrix
  if ( $n == 1 ) {
    $family_total->{ $regions[0]->family() }++;    # Still add to family counts
    return ();
  }

  for ( my $i = 0 ; $i < $n ; $i++ ) {

    # For every region add to family count
    if ( !$regions[$i] ) {
      warn "No region for $i\n";
    }

    $family_total->{ $regions[$i]->family() }++;

    for ( my $j = $i + 1 ; $j < $n ; $j++ ) {
      if ( !$regions[$j] ) {
        warn "No region for $j\n";
      }

      # Are regions the same?
      if ( $regions[$i]->overlap( $regions[$j] ) ) {

# Also test whether either region has counted towards a match between these families already.
        if (  !$regions[$i]->match( $regions[$j]->family() )
          and !$regions[$j]->match( $regions[$i]->family() ) )
        {

          # OK increment counts here
          $uber_matrix->{ $regions[$i]->family() }{ $regions[$j]->family() }++;

          # Add to list of outputs matched in region object
          $regions[$i]->add_match( $regions[$j]->family() );
          $regions[$j]->add_match( $regions[$i]->family() );
        }
      }
    }
  }
  return ();
}

