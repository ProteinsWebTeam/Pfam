package Bio::Rfam::View::Plugin::SecondaryStructure;

use Data::Dumper;
use Moose;
with 'MooseX::Role::Pluggable::Plugin';
use IO::File;


has foo => (
  is  => 'rw',
  isa => 'Int'
);

sub process {
  my $self = shift;
 # print 'Work on this ' . $self->parent->family->SEED->path . "\n";

  $self->findMotifs;
}


#Make coloured base pair diagrams
#
sub makeBling {
	my ($self) = @_;
	my $rfamdb = $self->parent->config->rfamlive;
	my $rfam_acc = $self->parent->family->DESC->AC;
}


# Main Subroutine that peforms scan etc
sub findMotifs {
	my ($self) = @_;
	
	my $rfamdb      = $self->parent->config->rfamlive;
	my $rfam_acc    = $self->parent->family->DESC->AC;

        # Confirm the family exists in the database
        my $famRow = $rfamdb->resultset('Family')->find( { rfam_acc => $rfam_acc } );
        if (!defined($famRow)) {
                croak ("Failed to find entry in the Family table for $rfam_acc.");
        }
	
	# Set/make locations 
        my $location    = "/nfs/production/xfam/rfam";
	my $results_loc = "$location/MOTIFS/results/$rfam_acc";
        File::path::make_path($results_loc);
        my $SEED        = "$results_loc/SEED";
        my $CMdb	= "$location/MOTIFS/cmdb/CM";        

        # Write the seed to file
	my $msa = $self->parent->family->SEED;
        $msa->write_msa($SEED);
	
        # Values used in the filtering of the SEED and inclusion cutoff
        my $fractionMotifs   = 0.1;
        my $minNumberHits    = 2; 
        my $idf              = 0.9;

        # Perform filtering, calculate weights & generate an array of seq ids that pass filter
        my ($filtSEED, $weights, $sum)=stockholm2filteredSTK($seed_loc,$idf,$seed_loc);
        my $filtSeqIDs = "$results_loc/filtSeqIDs";
        my @filtSeqIDs=stockholm2SeqIDs($filtSeqIDs, $filtSEED);
        unlink ($filtSeqIDs)
        
        # Create an array containing the accession of each motif in the database
        my @allMotifs;
        my @completeMotResultSet = $rfamdb->resultset('Motif')->all();
        foreach my $mot (@completeMotResultSet) {
          my $motif_acc = $mot->get_column('motif_acc');
        push(@allMotifs, $motif_acc);
        }
 
        # Run cmscan on the SEED
        my $cmscanOpts  = "--cpu 4 --max --toponly --verbose --cut_ga --tblout $results_loc/TBL -o $results_loc/cmscan"; 
        my $infernal    = $self->parent->config->infernalPath;
        my $command     = $infernal . "cmscan";
        my @args = ($command, $cmscanOpts, $CMdb, $SEED);
        system(@args) == 0 
          or die "System @args failed: $?";
        
        # Parse the TBL, and calculate statistics on those seq ids which make up the filtered SEED
        @filteredMoitfMatches = parseTBL2MotifMatchObj($TBL,$rfam_acc, @all_motifs, @filtSeqIDs);

        # Perform Calculations on filtered matches and print the results to filtered outlist
        my $filtMotifHash = motifMatchCalcs(@filteredMoitfMatches)
        printMotifOutlist(%filtMotifHash, "filt.outlist", $results_loc);
        
        # Create a list of motifs that fufill the heuristic criteria for accepting matches
        my @acceptedMotifs;
        foreach my $motif (keys %filtMotifHash) {
          if ($filtMotifHash{$motif}->{NUM_HITS} >= $minNumberHits && $filtMotifHash{$motif}->{FREQ_HITS} >= $fractionMotifs) {
           push (@acceptedMotifs, $motif);
          }
        }

        # Parse the TBL, and calculate statisitcs of accepted motif matched in the full SEED
        my $allSeqIDs = "$results_loc/allSeqIDs";
        my @allSeqIDs = stockholm2SeqIDs($allSeqIDs, $SEED); 
        unlink($allSeqIDs);
        @acceptedMoitfMatches = parseTBL2MotifMatchObj($TBL,$rfam_acc, @acceptedMotifs, @allSeqIDs);

        # Calculate statistics, print summary and send allowed matches into RfamLive
        my $acceptedMotifHash = motifMatchCalcs(@acceptedMoitfMatches)
        printMotifOutlist(%acceptedMotifHash, "allow.outlist", $results_loc);

        foreach my $motifMatchObj (@acceptedMotifMatches) {
          my $guard = $rfamdb->txn_scope_guard;
          $rfamdb->resultset('MotifMatch')->find_or_createFromMotifMatchObj($motifMatchObj);
          $guard->commit;

        # Calculate statistics, print summary of ALL matches (EVERY motif vs EVERY seq)
        @completeMoitfMatches = parseTBL2MotifMatchObj($TBL,$rfam_acc, @allMotifs, @allSeqIDs);
        my $completeMotifHash = motifMatchCalcs(@completeMoitfMatches)
        printMotifOutlist(%completeMotifHash, "complete.outlist", $results_loc);

        # Create a hash of motif labels (chars) for the seed markup (eg tetraloop = t)
        my (%motifLabels, %taken)
        foreach my $mot (@completeMotResultSet) {
          my $labMotifAcc = $mot->get_column('motif_acc');
          my $labMotifId  = $mot->get_column('motif_id');   
          $motifLabels{%labMotifId} = assign_motif_label($labMotifId,\%taken) if not defined $motifLabels{$labMotifId};
        }
 
        # Create an array of hashes for the markup
        my %f2=matchObjects2hash(@acceptedMotifMatches);

        # Markup the seed alignment with the accepted motifs
        markup($seed, /$f2, @acceptedMotifs, %motifLabels, %completeMotifHash, $dir);

}       

#---------------------------------------------------------------------------------------
# Perform calculations on an array of match objects, return a hash containing the results
sub motifMatchCalcs {
  my @motifMatchObjects = @_; 

  my %motifHash;
  my @completeMotResultSet = $rfamdb->resultset('Motif')->all();
  foreach my $mot (@completeMotResultSet) {
    my $motif_acc = $mot->get_column('motif_acc');
    $motifsHash{$motif_acc} = { NUM_HITS => 0, FREQ_HITS => 0, SUM_BITS => 0, AVG_W_BITS => 0, MOTIF_ID => $mot->get_column('motif_id')};
  
  foreach $motifMatchObj (@motifMatchObjects) {
    my $motif_acc = = $motifMatchObj->MOTIF_ACC;
    $motifHash{$TBL_motif_acc}->{NUM_HITS}+=1;
 
    $motifHash{motif_acc}->{SUM_BITS}+=sprintf("%.3f",$motifMatchObj->BIT_SCORE);

    my $seq_id = $motifMatchObj->RFAMSEQ_ID
    my $seq_weight;
    if (defined $weights->{$seq_id}) {
      $seq_weight = $weights->{$seq_id};
    }
    else { $seq_weight = 1.0 }
    if ($sum == 0) { $sum = 1.0}

    my $avg_w_bits=sprintf("%.3f",(($seq_weight*$motifMatchObj->BIT_SCORE)/$sum));
    $motifHash{$motif_acc}->{AVG_W_BITS}+=sprintf("%.3f",$avg_w_bits);
  }
  
  foreach my $motif (keys %motifHash) {
    my $numHits = ($motifHash{$motif}->{NUM_HITS}); 
    my $freq=$num_hits/$num_seq;
    $motifHash{$motif}->{FREQ_HITS}=sprintf("%.3f",$freq);

  return $motifHash;
}
#----------------------------------------------------------------------------------------
sub printMotifOutlist {
  my (%motifsHash, $outlistFname, $outlistDir) = @_;

  my $outlist = "$outlistDir/$outlistFname";

  open (my $outlistHandle, '>', $outlist) or die "Could not open file $outlist $!";
 
  printf $outlistHandle (    "%-12s %-12s %12s %15s %20s %25s\n", 
                             "rfam acc", 
                             "motif acc", 
                             "number hits", 
                             "fraction hits", 
                             "sum bits", 
                             "average weighted bits");
  
  foreach my $motif (keys %motifsHash) {
    if ($motifsHash{$motif}->{NUM_HITS} >= 1) {
      printf $outlistHandle (  "%-12s %-12s %12s %15s %20s %25s\n", 
                               $rfam_acc, $motif, 
                               $motifsHash{$motif}->{NUM_HITS}, 
                               $motifsHash{$motif}->{FREQ_HITS}, 
                               $motifsHash{$motif}->{SUM_BITS}, 
                               $motifsHash{$motif}->{W_SUM_BITS});
    }
  }
close $outlistHandle;
}
  

#----------------------------------------------------------------------------------------
# Parse the TBL out to an array of match objects
sub parseTBL2MotifMatchObj{
  my ($TBL, $rfam_acc, @motifs, @seqids) = @_;
  my @MotifMatchObjs;
  
  open(TBL, "grep -v ^'#' $TBL | sort -nrk 15 | ") || croak "FATAL: could not open pipe for reading $TBL\n[$!]";

  while (my $tblline = <$TBL>) {
    my @tblA = split(/\s+/, $tblline);
    my (   $motif_name, 
           $qName, 
           $CMstart, 
           $CMend, 
           $qStart, 
           $qEnd, 
           $strand, 
           $trunc, 
           $bits, 
           $evalue ) = ($tblA[0], $tblA[2], $tblA[5], $tblA[6], $tblA[7], $tblA[8], $tblA[9], $tblA[10], $tblA[14], $tblA[15]); 
  
    my $motif_acc_rs = $rfamlive->resultset('Motif')->search({ motif_id => $motif_id });
    my $new_motif_acc = $motif_acc_rs->get_column('motif_acc')->single(); 

    if ($new_motif_acc ~~ @motifs && $qName ~~ @seqids) {
    
      # Split rfamseq id into acc, start and stop
      my ($rfamseq_acc, $rfamseq_start, $rfamseq_stop);
      if ($rfamseq_id =~ /(\S+.\d)\/(\d+)-(\d+)/) {
        $rfamseq_acc, $rfamseq_start, $rfamseq_stop = $1, $2, $3;
      }

      # Create the match object
      my $motifMatchObj = Bio::Rfam::MotifMatch->new;
      $motifMatchObj->MOTIF_ACC($new_motif_acc);
      $motifMatchObj->RFAM_ACC($rfam_acc);
      $motifMatchObj->RFAMSEQ_ACC($rfamseq_acc);
      $motifMatchObj->RFAMSEQ_START($rfamseq_start);
      $motifMatchObj->RFAMSEQ_STOP($rfamseq_stop);
      $motifMatchObj->QUERY_START($qStart);
      $motifMatchObj->QUERY_STOP($qEnd);
      $motifMatchObj->MOTIF_START($CMstart);
      $motifMatchObj->MOTIF_STOP($CMend);
      $motifMatchObj->E_VALUE($evalue);
      $motifMatchObj->BIT_SCORE($bits);

      push(@aMotifMatchObjs, $motifMatchObj)

      }
    }
  return (@MotifMatchObjs);
}

#-------------------------------------------------------------------------------------------
# Assign labels to each motif to be used in markup of the SEED
sub assign_motif_label {
  my ($motif_id, $taken)=@_;
  my @chars = qw(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z a b c d e f g h i j k l m n o p q r s t u v w x y z 1 2 3 4 5 6 7 8 9 0);
  my @motif_ids = split(//,$motif_id);

  foreach my $c (@motif_ids,@chars) {
    next if ($c !~ /^[a-zA-Z0-9]*$/);
    return $c if (not defined $taken->{$c});
    }
}

#-----------------------------------------------------------------------------------------
# Creates an array of hashes from motif match objects to be used in the markup sub.
# This is not elegant and is only done to enable easy intergration of Paul's markup code
# which is not easy to decifer, however it does function.

sub matchObjects2hash {
  my @arrayMatchObjs = @_; 
 
  my %f2;
  foreach my $motifMatchObj (@arrayMatchObjs) {
    my $motifMatchSeqId = $motifMatchObj->RFAMSEQ_ID;
    
    my $seqid       = $motifMatchSeqId;
    my $start       = $motifMatchObj->QUERY_START;
    my $end         = $motifMatchObj->QUERY_STOP;
    my $strand      = 1;
    my $score       = $motifMatchObj->BIT_SCORE;
    my $evalue      = $motifMatchObj->E_VALUE;
    my $f_motif_acc = $motifMatchObj->MOTIF_ACC;
    my $label       = $motifLabels{$f_motif_acc};

    if( $end < $start ) {
      ( $start, $end ) = ( $end, $start );
      $strand = -1;
    }

    my %f = (  seqid           => $seqid,
               start           => $start,
               end             => $end,
               strand          => $strand,
               score           => $score,
               evalue          => $evalue,
               motif_acc       => $f_motif_acc,
               label           => $label );

  push( @{ $f2{$seqid} }, \%f );
  }
  return %f2;
}

#----------------------------------------------------------------------------------------
# Markup the seed with the accepted motifs - adapted from Pauls markup in rmfam_scan.pl
sub markup {
  my ($seed, $features, @acceptedMotifs, %motifLabels, %motifHash, $dir) = @_;
  
  my (@seqCoords2alnCoords, %positions2seqid, %seqid2positions, %motifLines, %motiffedSeqLineNumbers, $cnt=0, $firstSeqLine);

  my $alnLength = compute_length_of_alignment($seed);
  open (F, "esl-reformat -ru --mingap pfam $seed | ") or die "FATAL: could not open pipe for reading $STK\n[$!]"; 
  my @stk =<F>

  foreach my $stk (@stk){
    if($stk=~/^(\S+)\s+\S+$/){
      $positions2seqid{$cnt}=$1;
      $seqid2positions{$1}=$cnt;
      $firstSeqLine=$cnt if not defined $firstSeqLine;
    }
    $cnt++;
  }
  
  foreach my $seqid (keys %seqid2positions) {
    if(defined $features->{$seqid}){   
      my $fpos=0;
      foreach my $f ( @{$features->{$seqid}} ){
        my $alnSeq=$stk[$seqid2positions{$seqid}];
        if($alnSeq=~/^(\S+)\s+(\S+)$/){
          my ($lid,$lseq)=($1,$2);  
          if($lid ne $seqid){
	    print "WARNING: seqId:[$seqid] and alnSeq:[$alnSeq] don't match!\n";
            next;
	  }
          $alnSeq=$lseq; 
          my @alnSeq = split(//, $alnSeq);
          if(scalar(@alnSeq) != $alnLength){
	    printf "WARNING: the lengths [$alnLength]!=[%d] computed from seqId [$seqid] and alnSeq:\n[$alnSeq]\ndon't match! [CHECK: could be a gap-only column]\n", scalar(@alnSeq);
          }
          my ($aCnt,$sCnt)=(0,0); 
          foreach my $as (@alnSeq){
            if(is_nucleotide($as)){
              $seqCoords2alnCoords[$sCnt]=$aCnt;
              $sCnt++;
            }
            $aCnt++;
          }
        } 
        else { printf "WARNING: line number [%d] is meant to correspond to $seqid! Check the formatting.\n", $seqid2positions{$seqid};
        }
        my$mtCnt=0;
        my ($start,$end, $char, $rmfamid) = ($f->{'start'}, $f->{'end'}, $f->{'label'}, $f->{'motif_acc'});
        for (my $gpos=0; $gpos<$fpos; $gpos++){
          my $g = ${ $features->{$seqid} }[$gpos];
          $mtCnt++ if ( overlap($start,$end, $g->{'start'},$g->{'end'}) );
        }
        $motifLines{$seqid}[$mtCnt] = '.' x $alnLength if not defined $motifLines{$seqid}[$mtCnt];
        for(my $mpos=$start-1; $mpos<$end; $mpos++){
          my $aCoord = $seqCoords2alnCoords[$mpos];
          substr($motifLines{$seqid}[$mtCnt],$aCoord,1)=$char;
        }
        $fpos++;
      }
      $motiffedSeqLineNumbers{$seqid2positions{$seqid}}=$seqid if defined($motifLines{$seqid}[0]);
    }
  }

  my $outFile="$dir/tmp.annotated.SEED";
  my $fileh = IO::File->new();
  $fileh->open("> $outFile");
  for(my $ii=0; $ii<scalar(@stk); $ii++){
    if ($ii == $firstSeqLine-1){
      foreach my $l (sort {$motifLabels->{$a} cmp $motifLabels->{$b}} keys %{$motifLabels}){
        if ($l ~~ @acceptedMotifs){
          my $labMotifID = $motifHash{$l}->{MOTIF_ID};
          printf $fileh "#=GF MT.%s   %s   %s\n", $motifLabels->{$l}, $l, $labMotifID; 
        }
      }
    }
    print $fileh $stk[$ii];
    if (defined $motiffedSeqLineNumbers{$ii} && defined $positions2seqid{$ii}){
      my $mCnt=0;
      foreach my $mt (@{$motifLines{$positions2seqid{$ii}}}){
        printf $fileh "#=GR %s MT.$mCnt %s\n", $motiffedSeqLineNumbers{$ii}, $mt if (defined $mt);
        $mCnt++
      }
    }
  }
  my $annotatedSEED="$dir/annotated.SEED";
  system "esl-reformat stockholm $outFile > $annotatedSEED";
  unlink($outFile);
}

} 
#-----------------------------------------------------------------------------------------
sub overlap {
  my($x1, $y1, $x2, $y2) = @_;
  if ( ($x1<=$x2 && $x2<=$y1) || ($x1<=$y2 && $y2<=$y1) || ($x2<=$x1 && $x1<=$y2) || ($x2<=$y1 && $y1<=$y2)  ){
    return 1;
  }
  else {
    return 0;
  }
}
#-----------------------------------------------------------------------------------------
sub stockholm2filteredSTK {
    my ($infile,$idf,$fam_dir) = @_;
    system "esl-weight -f --idf $idf $infile > $fam_dir"."filtered.SEED"
	and die "FATAL: failed to run [esl-weight -f --idf $idf $infile > $fam_dir"."filtered.SEED";
    my ($weights,$sum) = stockholm2weights("$fam_dir"."filtered.SEED"); 
    
    return ("$fam_dir"."filtered.SEED",$weights,$sum);
}
#-----------------------------------------------------------------------------------------
sub stockholm2weights {
    my $infile = shift;
    my $sum=0;
    my %weights;
    open(F, "esl-weight -g $infile | ") or die "FATAL: could not open pipe for [esl-weight -g $infile]\n[$!]";
    while(my $w = <F>){
	if($w=~/^#=GS\s+(\S+)\s+WT\s+(\S+)/){
	    $weights{$1}=$2; 
	    $sum+=$2;
	}
    }
return \%weights, $sum;
}
#-----------------------------------------------------------------------------------------
sub stockholmSeqStats{
  my $infile = shift;
  my $seq_num;
  open(F, "esl-seqstat $infile | ") or die "FATAL: could not open pipe [esl-seqstat $infile]\n[$!]";
  while (my $w = <F>) {
    if($w=~/(^Number of sequences:)(\s+)(\d+)/) {
      $seq_num = $3;
     }
  }
  return $seq_num;
}
#----------------------------------------------------------------------------------------
sub stockholm2SeqIDs{
  my ($seqIDsList, $seed) = @_;  
  my @seqIDs;
  system("esl-alistat --list $seqIDsList $seed") == 0 or die "System esl-alistat failed: $?";
  open my $seqIDsHandle, $seqIDsList or die "Could not open $seqIDsList: $!";
  while (  my $line = <$seqIDsHandle>) {
           chomp($line);
           push(@seqIDs, $line);
  }
}
#-----------------------------------------------------------------------------------------
sub compute_length_of_aligment {    
  my $file = shift;
  my $alnLength;
  open(ALI,"esl-alistat --rna $file |") or die "FATAL: could not open [esl-seqalistat $file] pipe:[$!]";
  while(<ALI>) {
    if (/^Alignment length:\s+(\d+)/){ 
      $alnLength=$1;
    }
  }
  close(ALI);
  print "WARNING: alnLength is undefined for [esl-alistat $file]!" if not defined $alnLength;
  return $alnLength;
}

#-----------------------------------------------------------------------------------------
sub is_nucleotide {
  my $a = shift;
  if (defined($a)){
    $a =~ tr/a-z/A-Z/;
  }  
  if (defined($a) && length($a) && ($a =~ /[ACGUTRYWSMKBDHVN]/) ){
    return 1;}
  else {return 0;}
}
#-----------------------------------------------------------------------------------------

1;
