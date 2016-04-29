package Bio::Pfam::ViewProcess::Scoop;

use strict;
use warnings;
use Net::SCP;
use Moose;
use Moose::Util::TypeConstraints;
use Bio::SCOOP::Region;
use Bio::SCOOP::RegionSet;
use DDP;
use File::Slurp;

extends 'Bio::Pfam::ViewProcess::Architecture';

has '+statusFile' => (
  default => 'runScoop',
);

sub runScoop {
  my ( $self ) = @_;
  
  my $dbh = $self->pfamdb->getSchema->storage->dbh;
#  my $scp = Net::SCP->new( { "host"=> $self->config->{Model}->{Pfamlive}->{host} } );
  my $statusdir = $self->options->{statusdir};
  my $user =  $self->config->{Model}->{Pfamlive}->{user};
  my $pass =  $self->config->{Model}->{Pfamlive}->{password};
  my $port =  $self->config->{Model}->{Pfamlive}->{port};
  my $host =  $self->config->{Model}->{Pfamlive}->{host};
  my $pfamdb = $self->config->{Model}->{Pfamlive}->{database};
  if(!$self->statusCheck("sigReg.txt")){
    $self->logger->info('Getting significant regions');
    my $cmd = "mysql -h $host -u $user -p$pass -P $port --skip-column-names $pfamdb --quick -e \"SELECT pfamseq_acc, pfamA_acc, seq_start, seq_end, domain_evalue_score FROM pfamA_reg_full_significant\" > $statusdir/sigReg.txt";
    system($cmd) and die "Could not execute statement $cmd\n";
  }
  
  if(!$self->statusCheck("insigReg.txt")){
    $self->logger->info('Getting insignificant regions');
    my $cmd = "mysql -h $host -u $user -p$pass -P $port --skip-column-names $pfamdb --quick -e \"SELECT pfamseq_acc, pfamA_acc, seq_start, seq_end, domain_evalue_score FROM pfamA_reg_full_insignificant\" > $statusdir/insigReg.txt";
    system($cmd) and die "Could not execute statement $cmd\n";
  }
  
  #Now sort the regions
  if(!$self->statusCheck("allReg.txt")){
    system("cat ".$statusdir."/sigReg.txt ".$statusdir."/insigReg.txt >".$statusdir."/allReg.txt");
  }
  if(!$self->statusCheck("allRegSort.txt")){
    system("sort ".$statusdir."/allReg.txt > ".$statusdir."/allRegSort.txt") 
       and $self->logger->logdie("Failed to sort the regions:[$!]");
  }
  
   if(!$self->statusCheck("runScoop")){
    open( my $upload, '>', $statusdir."/scoopRes.4upload.txt" )
      or $self->logger->logdie("Failed to open file $statusdir/scoopRes.4upload.txt:[$!]");
    $self->logger->debug("Running scoop"); 
    $self->scoop($statusdir."/allRegSort.txt", $upload, $statusdir);
    close($upload);
    $self->touchStatus("runScoop");
   }
  
  $self->pfamdb->getSchema->resultset('PfamA2pfamAScoop')->delete;

  #upload results to database
  my @results = read_file("$statusdir/scoopRes.4upload.txt");
  foreach my $line (@results){
      my @data = split(/\s+/, $line);
      $self->pfamdb->getSchema->resultset('PfamA2pfamAScoop')->update_or_create(
          {
              pfama_acc_1 => $data[0],
              pfama_acc_2 => $data[1],
              score => $data[2]

          }
      );
  }

}

sub scoop {
  my ( $self, $file, $fh, $tempDir ) = @_;

  $self->logger->info("Reading regions");
  my $total_region = 0;
  my %family_total;    # Counts matches per family
  my %uber_matrix;

  open( FH, $file ) or die "Cannot open $file";
  open( FHO, ">$tempDir/old_scores" ) or die "cannot write $tempDir/old_scores:[$!]";
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

    if(!defined($old_id)){
      $set = new Bio::SCOOP::RegionSet($id);
      $set->add($region);
      $total_region++;

    }elsif ( $id ne $old_id ) {
      # Now add old set to uber_matrix
      $self->_add_set_to_matrix( $set, \%family_total, \%uber_matrix );
      
      # Make new set
      $set = new Bio::SCOOP::RegionSet($id);
      $set->add($region);
      $total_region++;
    }
    elsif ( $id eq $old_id ) {
      $set->add($region);
      $total_region++;
    }
    $old_id = $id;
  }
  $self->logger->info("Finished processing regions");

  # Now do final processing
  $self->logger->info( "Total regions:$total_region");

  # Variables used to calculate the average score per family
  my %family_score_total;
  my %family_score_denominator;

  # Loop over every pair of families in uber matrix and calculate score

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

      print FHO "$score $x $y $family_total{$y} Both $observed\n";

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
    print FHO
"# Average score for $x is $average_score denominator is $family_score_denominator{$x}\n";
  }
  print FHO "# TOTAL REGIONS $total_region\n";
  close FHO;

  $self->logger->info("Second pass over Uber matrix\n");
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
sub _add_set_to_matrix {
  my ( $self, $set, $family_total, $uber_matrix ) = @_;
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
  return;
}


1;
