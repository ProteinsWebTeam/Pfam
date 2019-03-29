package Bio::Pfam::MPfam;


sub store_regions {
  my ($famObj, $regions) = @_;

  #Store seed sequences in regions hash
  foreach my $seq ( $famObj->SEED->each_seq ) {
    push @{ $regions{$seq->id} },
    { 
      from      => $seq->start,
      to        => $seq->end,
      family    => ( $famObj->DESC->AC ? $famObj->DESC->AC : $family ),
      ali       => 'SEED',
      family_id => ( $famObj->DESC->ID ? $famObj->DESC->ID : "NEW" )
    };
  }

  #Store full sequences in regions hash
  foreach my $seq ( keys %{ $famObj->scores->regions } ) {
    my $id;
    if ( $seq =~ /(\S+)\.\d+/ ) {
      $id = $1;
    }
    else {
      $id = $seq;
    }

    foreach my $fullReg ( @{ $famObj->scores->regions->{$seq} } ) {
      push @{ $regions{$id} },
      { 
        ali_from  => $fullReg->{aliStart},
        ali_to    => $fullReg->{aliEnd},
        from      => $fullReg->{start},
        to        => $fullReg->{end},
        score     => $fullReg->{score},
        evalue    => $fullReg->{evalue},
        family    => ( $famObj->DESC->AC ? $famObj->DESC->AC : $family ),
        family_id => ( $famObj->DESC->ID ? $famObj->DESC->ID : "NEW" ),
        ali       => 'FULL'
      };
    }
  }
}

sub print_overlaps {
  my ($overlaps) = @_;

  my $numOverlaps=0;
  my %seen;
  open(OVERLAPS, ">$family/overlap") or die "Couldn't open fh to $family/overlap, $!";
  foreach my $seqAcc ( keys %$overlaps ) {
    foreach my $region ( sort { $a->{from} <=> $b->{from} } @{ $overlaps{$seqAcc} } ) {

      foreach my $overRegion ( @{ $region->{overlap} } ) {

        my $line;
        if($region->{ali} eq 'SEED') {
          $line ="Sequence [". $seqAcc."] overlap ".$region->{family_id}." ".$region->{family}."/".$region->{from}."-".$region->{to}." ".$region->{ali}." with ";
        }
        else {
          $line ="Sequence [". $seqAcc."] overlap ".$region->{family_id}." ".$region->{family}."/".$region->{ali_from}."-".$region->{ali_to}." (".
          $region->{family}."/".$region->{from}."-".$region->{to}.", ".$region->{score}." bits) ".$region->{ali}." with ";
        }

        if($overRegion->{ali} eq 'SEED') {
          $line .=$overRegion->{family_id}." ".$overRegion->{family}."/".$overRegion->{from}."-".$overRegion->{to}." ".$overRegion->{ali}."\n";
        }
        else {
          $line .=$overRegion->{family_id}." ".$overRegion->{family}."/".$overRegion->{ali_from}."-".$overRegion->{ali_to}." (".$overRegion->{family}."/".$overRegion->{from}."-".$overRegion->{to}
          .", ".$overRegion->{score}." bits) ".$overRegion->{ali}."\n";
        }

        next if ( $seen{$line} );
        $seen{$line}++;

        $numOverlaps++;
        print STDERR $line;
        print OVERLAPS $line;
      } 
    }
  }
  print STDERR "Your family has $numOverlaps overlaps to the SEEDs of other families.\n";
}

sub update_pfamA {
  my ($famObj, $pfamDB) = @_;
  print STDERR "Updating pfamA\n";
  $pfamDB->getSchema->resultset('PfamA')->update_or_create(
    {
      pfama_acc       => $famObj->DESC->AC,
      pfama_id        => $famObj->DESC->ID,
      previous_id     => $famObj->DESC->PI,
      description     => $famObj->DESC->DE,
      deposited_by    => $ENV{'USER'},
      seed_source     => $famObj->DESC->SE,
      type            => $famObj->DESC->TP,
      comment         => $famObj->DESC->CC,
      sequence_ga     => $famObj->DESC->CUTGA->{seq},
      domain_ga       => $famObj->DESC->CUTGA->{dom},
      sequence_tc     => $famObj->DESC->CUTTC->{seq},
      domain_tc       => $famObj->DESC->CUTTC->{dom},
      sequence_nc     => $famObj->DESC->CUTNC->{seq},
      domain_nc       => $famObj->DESC->CUTNC->{dom},
      buildmethod     => $famObj->DESC->BM,
      model_length    => $famObj->HMM->length,
      searchmethod    => $famObj->DESC->SM,
      msv_lambda      => $famObj->HMM->msvStats->{lambda},
      msv_mu          => $famObj->HMM->msvStats->{mu},
      viterbi_lambda  => $famObj->HMM->viterbiStats->{lambda},
      viterbi_mu      => $famObj->HMM->viterbiStats->{mu},
      forward_lambda  => $famObj->HMM->forwardStats->{lambda},
      forward_tau     => $famObj->HMM->forwardStats->{tau},
      num_seed        => $famObj->SEED->num_sequences,
      num_full        => $famObj->scores->numRegions,
      updated         => \'NOW()'
    }
  );

}


sub update_pfamA_reg_seed {
  my ($famObj, $pfamDB) = @_;

  print STDERR "Updating pfamA_reg_seed\n";
  #Delete all the seed regions
  $pfamDB->getSchema->resultset('PfamARegSeed')->search( { pfamA_acc => $famObj->DESC->AC } )->delete;

  $pfamDB->getSchema->storage->dbh->do('SET foreign_key_checks=0');
  my $pfamA_acc=$famObj->DESC->AC;

  my @rows;
  foreach my $seq ( $famObj->SEED->each_seq ) {
    push(
      @rows,
      {    
        pfamseq_acc => $seq->id,
        pfama_acc   => $pfamA_acc,
        seq_start    => $seq->start,
        seq_end      => $seq->end
      }    
    );   
  }

  $pfamDB->getSchema->resultset('PfamARegSeed')->populate( \@rows );
  $pfamDB->getSchema->storage->dbh->do('SET foreign_key_checks=1');
}


sub update_pfamA_reg_full {
  my ($famObj, $pfamDB) = @_; 

  print STDERR "Updating pfamA_reg_full\n";

  #Delete old regions
  $pfamDB->getSchema->resultset('PfamARegFullSignificant')->search( { pfamA_acc => $famObj->DESC->AC } )->delete;

  $pfamDB->getSchema->storage->dbh->do('SET foreign_key_checks=0');

  my @significant; 
  foreach my $seq ( @{ $famObj->PFAMOUT->eachHMMSeq } ) {
    next unless ($seq);

    if ( $seq->bits >= $famObj->DESC->CUTGA->{seq} ) {
      foreach my $u ( @{ $seq->hmmUnits } ) {

        #Is it significant dom?
        if ( $u->bits >= $famObj->DESC->CUTGA->{dom} ) {

          push(
            @significant,
            {    
              pfama_acc   => $famObj->DESC->AC,
              pfamseq_acc => $seq->name,
              seq_start    => $u->envFrom,
              seq_end      => $u->envTo,
              ali_start    => $u->seqFrom,
              ali_end               => $u->seqTo,
              model_start           => $u->hmmFrom,
              model_end             => $u->hmmTo,
              domain_bits_score     => $u->bits,
              domain_evalue_score   => $u->evalue,
              sequence_bits_score   => $seq->bits,
              sequence_evalue_score => $seq->evalue,
            }    
          );   
        }    

      }
    }
    if ( scalar(@significant) > 1000 ) {
      $pfamDB->getSchema->resultset("PfamARegFullSignificant")->populate( \@significant );
      @significant = ();
    }
  }

  $pfamDB->getSchema->resultset("PfamARegFullSignificant")->populate( \@significant );
  $pfamDB->getSchema->storage->dbh->do('SET foreign_key_checks=1');
}




sub upload_HMM {
  my ($famObj, $family, $pfamDB) = @_;

  print STDERR "Uploading HMM\n";

  #Need to add some info to the HMM before uploading
  open( HMM, "$family/HMM") or die "Couldn't open fh to $family/HMM, $!\n";
  my $hmm_string;
  while (<HMM>) {
    if(/^(ACC|DESC|GA|TC|NC|BM|SM)/) {
      next;
    }
    elsif(/^(NAME\s+)/) {
      $hmm_string .= $1.$famObj->DESC->ID."\n";
      $hmm_string .= "ACC   ".$famObj->DESC->AC."\n";
      $hmm_string .= "DESC  ".$famObj->DESC->DE."\n";
    }
    elsif(/^CKSUM/) {
      $hmm_string .= $_;
      $hmm_string .= "GA    ".$famObj->DESC->CUTGA->{seq}." ".$famObj->DESC->CUTGA->{dom}.";\n";
      $hmm_string .= "TC    ".$famObj->DESC->CUTTC->{seq}." ".$famObj->DESC->CUTTC->{dom}.";\n";
      $hmm_string .= "NC    ".$famObj->DESC->CUTNC->{seq}." ".$famObj->DESC->CUTNC->{dom}.";\n";
      $hmm_string .= "BM    ".$famObj->DESC->BM."\n";
      $hmm_string .= "SM    ".$famObj->DESC->SM."\n";
    }
    else {
      $hmm_string .= $_; 
    }
  }
  close HMM;

  $pfamDB->getSchema->resultset('PfamAHmm')->update_or_create(
    {
      pfama_acc  => $famObj->DESC->AC,
      hmm        => $hmm_string
    }
  );

}

sub upload_seed {
  my ($famObj, $family, $pfamDB) = @_; 

  print STDERR "Uploading seed\n";

  #Need to add some info to the HMM before uploading
  open(SEED, "$family/SEED") or die "Couldn't open fh to $family/SEED, $!\n";
  my $seed_string;
  while (<SEED>) {
    unless(/^\/\//) {
      $seed_string .= $_;
    }
  }
  close SEED;

  $pfamDB->getSchema->resultset('PfamASeed')->update_or_create(
    {   
      pfama_acc  => $famObj->DESC->AC,
      seed       => $seed_string
    }   
  );  

}

1;
