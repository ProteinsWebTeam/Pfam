package Bio::Pfam::MGnifam;


=head2 store_regions

 Title    : store_regions
 Usage    : store_regions($famObj, \%regions)
 Function : Store the regions from a SEED and FULL alignment in the regions hash   
 Returns  : Nothing
 Args     : Bio::Pfam::Family object and a hash reference

=cut

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


=head2 getOverlapingSeedPfamRegions

 Title    : getOverlapingSeedPfamRegions (modified from subroutine of the same name in PfamDBManager.pm)
 Usage    : getOverlapingSeedPfamRegions($pfamDB, \%regions, \%overlaps)
 Function : Puts any regions that overlap with the mgnifam_reg_seed table into the overlaps hash
 Returns  : Nothing
 Args     : Bio::Pfam::PfamLiveDBManager object, hash reference, hash reference 

=cut

sub getOverlapingSeedPfamRegions {
  my ( $pfamDB, $regionsHash, $overlaps ) = @_;

  my $dbh = $pfamDB->getSchema->storage->dbh;
  my $sth = $dbh->prepare(
    "select distinct seq_start, seq_end, a.mgnifam_acc, mgnifam_id from mgnifam a, mgnifam_reg_seed r where r.mgnifamseq_acc=? and
    ((? >= r.seq_start and ? <= r.seq_end) or ( ? >= r.seq_start and ? <= r.seq_end) or (? < r.seq_start and ? >r.seq_end))
      and r.mgnifam_acc=a.mgnifam_acc and a.mgnifam_acc != ?"
  ) or confess $dbh->errstr;

  foreach my $seqAcc ( keys %{$regionsHash} ) {
    my %seen;
    foreach my $region ( @{ $regionsHash->{$seqAcc} } ) {

      #Use start, ends for seed, and alignment co-ordinates for full
      my ($st, $en);
      if($region->{ali} eq "SEED") {
        $st=$region->{from};
        $en=$region->{to};
      }    
      else {
        $st=$region->{ali_from};
        $en=$region->{ali_to};
      }    
      unless($st and $en) {
        die "$seqAcc ". $region->{ali} . " has no start/end\n";
      }    

      next if($seen{"$st:$en:".$region->{ali}});
      $seen{"$st:$en:".$region->{ali} }++; 

      $sth->execute($seqAcc, $st, $st, $en, $en, $st, $en, $region->{family});
      my $foundOverlap = 0; 
      foreach my $row ( @{ $sth->fetchall_arrayref } ) {
        push(
          @{ $region->{overlap} },
          {    
            from      => $row->[0],
            to        => $row->[1],
            family    => $row->[2],
            family_id => $row->[3],
            ali       => 'SEED'
          }    
        );   
        $foundOverlap++;
      }    
      push( @{ $overlaps->{$seqAcc} }, $region ) if ($foundOverlap);
    }    
  }
}


=head2 print_overlaps

 Title    : print_overlaps
 Usage    : print_overlaps(\%overlaps)
 Function : Print the overlaps contained in the \%overlaps hash to STDERR and an overlaps file. To be used after getOverlapingSeedPfamRegions subroutine
 Returns  : Nothing
 Args     : Hash reference containing any overlaps

=cut

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


=head2 add_mgnifam

 Title    : add_mgnifam
 Usage    : add_mgnifam($famObj, $pfamDB)
 Function : Adds a new entry to the mgnifam table in database
 Returns  : Nothing (populates table in the database)
 Args     : Bio::Pfam::Family object, Bio::Pfam::PfamLiveDBManager object

=cut

sub add_mgnifam {
  my ($famObj, $pfamDB) = @_; 
  print STDERR "Updating mgnifam\n";

  my $au_line;
  foreach my $author (@{$famObj->DESC->AU}) {
    $au_line .= ", " if($au_line);
    $au_line .= $author->{name};
  }
  
  $pfamDB->getSchema->resultset('Mgnifam')->update_or_create(
    {
      mgnifam_acc     => $famObj->DESC->AC,
      mgnifam_id      => $famObj->DESC->ID,
      previous_id     => $famObj->DESC->PI,
      description     => $famObj->DESC->DE,
      author          => $au_line,
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
      created         => \'NOW()',
      updated         => \'NOW()'
    }
  );
}


=head2 update_mgnifam

 Title    : update_mgnifam
 Usage    : update_mgnifam($famObj, $pfamDB)
 Function : Updates mgnifam table in database 
 Returns  : Nothing (populates table in the database)
 Args     : Bio::Pfam::Family object, Bio::Pfam::PfamLiveDBManager object

=cut

sub update_mgnifam {
  my ($famObj, $pfamDB) = @_; 
  print STDERR "Updating mgnifam\n";

  my $au_line;
  foreach my $author (@{$famObj->DESC->AU}) {
    $au_line .= ", " if($au_line);
    $au_line .= $author->{name};
  }


  $pfamDB->getSchema->resultset('Mgnifam')->update_or_create(
    {
      mgnifam_acc     => $famObj->DESC->AC,
      mgnifam_id      => $famObj->DESC->ID,
      previous_id     => $famObj->DESC->PI,
      description     => $famObj->DESC->DE,
      author          => $au_line,
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


=head2 update_mgnifam_reg_seed

 Title    : update_mgnifam_reg_seed
 Usage    : update_mgnifam_reg_seed($famObj, $pfamDB)
 Function : Updates mgnifam_reg_seed table in the database
 Returns  : Nothing
 Args     : Bio::Pfam::Family object, Bio::Pfam::PfamLiveDBManager object

=cut

sub update_mgnifam_reg_seed {
  my ($famObj, $pfamDB) = @_;

  print STDERR "Updating mgnifam_reg_seed\n";
  #Delete all the seed regions
  $pfamDB->getSchema->resultset('MgnifamRegSeed')->search( { mgnifam_acc => $famObj->DESC->AC } )->delete;

  $pfamDB->getSchema->storage->dbh->do('SET foreign_key_checks=0');
  my $mgnifam_acc=$famObj->DESC->AC;

  my @rows;
  foreach my $seq ( $famObj->SEED->each_seq ) {
    push(
      @rows,
      {    
        mgnifamseq_acc => $seq->id,
        mgnifam_acc   => $mgnifam_acc,
        seq_start    => $seq->start,
        seq_end      => $seq->end
      }    
    );   
  }

  $pfamDB->getSchema->resultset('MgnifamRegSeed')->populate( \@rows );
  $pfamDB->getSchema->storage->dbh->do('SET foreign_key_checks=1');
}


=head2 update_mgnifam_reg_full

 Title    : update_mgnifam_reg_full
 Usage    : update_mgnifam_reg_full($famObj, $pfamDB)
 Function : Updates mgnifam_reg_full table in the database
 Returns  : Nothing
 Args     : Bio::Pfam::Family object, Bio::Pfam::PfamLiveDBManager object 

=cut

sub update_mgnifam_reg_full {
  my ($famObj, $pfamDB) = @_; 

  print STDERR "Updating mgnifam_reg_full\n";

  #Delete old regions
  $pfamDB->getSchema->resultset('MgnifamRegFull')->search( { mgnifam_acc => $famObj->DESC->AC } )->delete;

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
              mgnifam_acc   => $famObj->DESC->AC,
              mgnifamseq_acc => $seq->name,
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
      $pfamDB->getSchema->resultset("MgnifamRegFull")->populate( \@significant );
      @significant = ();
    }
  }

  $pfamDB->getSchema->resultset("MgnifamRegFull")->populate( \@significant );
  $pfamDB->getSchema->storage->dbh->do('SET foreign_key_checks=1');
}


=head2 upload_HMM

 Title    : upload_HMM
 Usage    : upload_HMM($famObj, $family_dir, $pfamDB)
 Function : Updates mgnifam_HMM table in database   
 Returns  : Nothing
 Args     : Bio::Pfam::Family object, family dir, Bio::Pfam::PfamLiveDBManager object 

=cut

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

  $pfamDB->getSchema->resultset('MgnifamHmm')->update_or_create(
    {
      mgnifam_acc  => $famObj->DESC->AC,
      hmm        => $hmm_string
    }
  );

}


=head2 upload_seed

 Title    : upload_seed
 Usage    : upload_seed($famObj, $family_dir, $pfamDB)
 Function : Updates mgnify_reg_seed table in database   
 Returns  : Nothing
 Args     : Bio::Pfam::Family object, family dir, Bio::Pfam::PfamLiveDBManager object

=cut

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

  $pfamDB->getSchema->resultset('MgnifamSeed')->update_or_create(
    {   
      mgnifam_acc  => $famObj->DESC->AC,
      seed       => $seed_string
    }   
  );  

}

=head2 next_MGDUF_number

 Title    : next_MGDUF_number
 Usage    : next_MGDUF_number($pfamDB)
 Function : Gets next MGDUF number (eg 10)
 Returns  : Next MGDUF number
 Args     : Bio::Pfam::PfamLiveDBManager object

=cut

sub next_MGDUF_number {

  my ($pfamDB) = @_;

  #Get a list of dufs from the database
  my %duf_numbers;
  my @fams = $pfamDB->getSchema->resultset("Mgnifam")->search();
  my @dead_fams = $pfamDB->getSchema->resultset("DeadMgnifam")->search();

  foreach my $mgnifam (@fams, @dead_fams){
    if($mgnifam->mgnifam_id =~ /^MGDUF(\d+)$/){
      $duf_numbers{$1}=1;
    }
  }

  #Find the biggest duf number, and increment by one
  my $num;
  foreach my $duf (sort { $b <=> $a } keys %duf_numbers) {
    $num = $duf + 1;
    last;
  }

  return $num;
}


1;
