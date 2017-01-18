package Bio::Pfam::ViewProcess::Scoop;

use strict;
use warnings;
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
    system("cat ".$statusdir."/sigReg.txt ".$statusdir."/insigReg.txt >".$statusdir."/allReg.txt") and $self->logger->logdie("Failed to cat regions:[$!]");
  }
  if(!$self->statusCheck("allRegSort.txt")){
    system("sort ".$statusdir."/allReg.txt > ".$statusdir."/allRegSort.txt") 
      and $self->logger->logdie("Failed to sort the regions:[$!]");
  }

  if(!$self->statusCheck("runScoop")){
    $self->logger->info('Running SCOOP');
    open( my $upload, '>', $statusdir."/scoopRes.4upload.txt" )
      or $self->logger->logdie("Failed to open file $statusdir/scoopRes.4upload.txt:[$!]");
    $self->scoop($statusdir."/allRegSort.txt", $upload, $statusdir);
    close($upload);
    $self->touchStatus("runScoop");
  }

  $self->pfamdb->getSchema->resultset('PfamA2pfamAScoop')->delete;

  #upload results to database
  my @results = read_file("$statusdir/scoopRes.4upload.txt");
  $self->logger->info('Uploading SCOOP results to database');
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
  my $total_residue=0;
  my $e_thresh=10;
  my %family_total; # Counts matches per family
  my %uber_matrix; # Counts number of matches between regions.
  my %sump_score_matrix; # Stores sum of probabilities of true matches

  open( FH, $file ) or die "Cannot open $file";
  #open( FHO, ">$tempDir/old_scores" ) or die "cannot write $tempDir/old_scores:[$!]";
  my $old_id;
  my ( $set, $new_set );
  foreach my $line (<FH>) {
    chomp $line;
    my @line = split( /\s+/, $line );
    my ($id, $family, $start, $end, $evalue) = ($line[0], $line[1], $line[2], $line[3], $line[4]);

    if ($evalue>$e_thresh){
      next;
    }

    # Make new region object
    my $region = new Bio::SCOOP::Region(
      'id'          => $id,
      'start'       => $start,
      'end'         => $end,
      'family'      => $family,
      'evalue'      => $evalue,
    );

    $total_residue+= $end-$start+1;

    if(!defined($old_id)){
      $set = new Bio::SCOOP::RegionSet($id);
      $set->add($region);
      $total_region++;

    }elsif ( $id ne $old_id ) {
      # Now add old set to uber_matrix
      $self->_add_set_to_matrix($set,\%family_total,\%uber_matrix,\%sump_score_matrix);

      # Make new set
      $set = new Bio::SCOOP::RegionSet($id);
      $set->add($region);
      $total_region++;  # This is counting number of regions! NB this is what original paper states.
    }
    elsif ( $id eq $old_id ) {
      $set->add($region);
      $total_region++;
    }
    $old_id = $id;
  }
  close FH;

  # OK now have all regions loaded up into matrices.
  $self->logger->info("Finished processing regions");

  # Now do final processing
  $self->logger->info("Total regions:$total_region");
  $self->logger->info("Total residues:$total_residue");

  # Loop over every pair of families in sum of probability matrix
  foreach my $x (sort keys %sump_score_matrix){
    foreach my $y (sort keys %{$sump_score_matrix{$x}}){
      my $score=$sump_score_matrix{$x}{$y};
      my $observed=$uber_matrix{$x}{$y};
      # print out raw scores
      print $fh "$x\t$y\t$score\n";
    }
  }
  close FH;

}


# This routine takes a RegionSet and does comparison and adds data to uber_matrix
sub _add_set_to_matrix {
  my ($self,$set,$family_total,$uber_matrix,$sump_score_matrix) = @_;

  my @regions=$set->each();
  my $n=@regions;

  # Deal with case of only a single region matching protein
  if ($n == 1){
    $family_total->{$regions[0]->family()}++; # Still add to family counts  $family_residue_total->{$regions[0]->family()}+=$regions[0]->{'end'}-$regions[0]->{'start'}+1; # Still add to family residue counts
    return;
  }

  # Now loop over diagonal half matrix
  for (my $i=0;$i<$n;$i++){
    # For every region add to family count
    if (! $regions[$i]){
      warn "No region for $i\n"
    }

    $family_total->{$regions[$i]->family()}++;

    for (my $j=$i+1;$j<$n;$j++){
      if (! $regions[$j]){
        warn "No region for $j\n";
      }
      # Are regions the same?
      if ($regions[$i]->overlap($regions[$j])){
        # Test whether either region has counted towards a match between these families already.
        # To avoid any region being counted multiple times
        if (! $regions[$i]->match($regions[$j]->family()) and ! $regions[$j]->match($regions[$i]->family())){
          $uber_matrix->{$regions[$i]->family()}{$regions[$j]->family()}++;
          $sump_score_matrix->{$regions[$i]->family()}{$regions[$j]->family()}+=exp(-$regions[$i]->{'evalue'})*exp(-$regions[$j]->{'evalue'});

          # Add to list of outputs matched in region object
          $regions[$i]->add_match($regions[$j]->family());
          $regions[$j]->add_match($regions[$i]->family());
        }
      }
    }
  }
  return();
}

1;
