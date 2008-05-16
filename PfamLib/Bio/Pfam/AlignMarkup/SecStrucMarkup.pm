package Bio::Pfam::AlignMarkup::SecStrucMarkup;

use strict;
use warnings;
use Log::Log4perl qw(get_logger);

my $logger = get_logger(__PACKAGE__);

sub getPfamADsspData {
  my ($autoPfamA, $filename, $pfamDB) = @_;
  $logger->debug("Going to fetch Secondary structure information for auto_pfamA $autoPfamA");
 
  my (%famDSSP, @dssp);
  if($filename eq 'ALIGN'){
      @dssp = $pfamDB->getSchema
                          ->resultset("Pdb_residue")
                            ->search({auto_pfamA => $autoPfamA,
                                      in_full    => 1 },
                                     {join => [qw(pfamseq pdb pfamA_reg_full_significant)],
                                      select => [qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number dssp_code)],
                                      as     => [qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number dssp_code)]});

  }elsif($filename eq 'SEED'){
          @dssp = $pfamDB->getSchema
                          ->resultset("Pdb_residue")
                            ->search({auto_pfamA => $autoPfamA},
                                      {join => [qw(pfamseq pdb pfamA_reg_seed)],
                                       select => [qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number dssp_code)],
                                       as     => [qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number dssp_code)]});
  }else{
    #mailUserAndFail($job, "Unknown file name passed in ($filename) to _getDsspData, expected ALIGN or SEED"); 
  }

  #Now stuff it into a data structure for working on
  my $acc2map;
  foreach (@dssp){
    $acc2map->{$_->get_column('pfamseq_acc')}
        ->{$_->get_column('pfamseq_seq_number')}
          ->{$_->get_column('pdb_id')."_".$_->get_column('chain')}
            ->{$_->get_column('pdb_seq_number')} = $_->get_column('dssp_code');
  }
  return ($acc2map);
}

sub getPfamBDsspData {
  my ($autoPfamB, $pfamDB) = @_;
  $logger->debug("Going to fetch Secondary structure information for auto_pfamB $autoPfamB");
 
  my (%famDSSP, @dssp);
  @dssp = $pfamDB->getSchema
                  ->resultset("Pdb_residue")
                    ->search({auto_pfamB => $autoPfamB},
                             {join   => [qw(pfamseq pdb pfamB_reg)],
                              select => [qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number dssp_code)],
                              as     => [qw(pfamseq_acc pfamseq_seq_number chain pdb_id pdb_seq_number dssp_code)]});


  #Now stuff it into a data structure for working on
  my $acc2map;
  foreach (@dssp){
    $acc2map->{$_->get_column('pfamseq_acc')}
        ->{$_->get_column('pfamseq_seq_number')}
          ->{$_->get_column('pdb_id')."_".$_->get_column('chain')}
            ->{$_->get_column('pdb_seq_number')} = $_->get_column('dssp_code');
  }
  return ($acc2map);
}


  
sub markupAlignWithSS{
  my ($aln, $dsspDataRef) = @_;
  my $noStructures;
  my $map;
  my @allSsStrings;

  foreach my $seq ($aln->each_seq()) {
    my $start = $seq->start;
    my $end = $seq->end;
    my @ali = split(//, $seq->seq);
    my %ss;
    my $p = $start;
    my %uniqueMaps;
    
    foreach my $pos (keys %{$$dsspDataRef{$seq->acc}}){
      foreach my $pdb_chain (keys %{$$dsspDataRef{$seq->acc}{$pos}}){
	       $uniqueMaps{$pdb_chain} = 1;
      }
    }

    foreach my $res (@ali){
      if($res eq "." or $res eq "-"){
	       #Add $res to the ssString
	       foreach my $pdb_chain (keys %uniqueMaps){
	         $ss{$pdb_chain}{ssString} .= "$res";
	       }
      }else{
      	#Okay, we have a residue
	       foreach my $pdb_chain (keys %uniqueMaps){
	         #Test to see if there is positional information for this residue?
	         if( values %{$$dsspDataRef{$seq->acc}{$p}{$pdb_chain}}){
	           while (my ($pdbResNum, $dsspCode) = each  %{$$dsspDataRef{$seq->acc}{$p}{$pdb_chain}}){
	             if($dsspCode =~ /\S+/){
		              $ss{$pdb_chain}{ssString} .= $dsspCode;
	             }else{
		              $ss{$pdb_chain}{ssString} .= "-";
	             }
	             if(! $ss{$pdb_chain}{start} || !$ss{$pdb_chain}{end}){
		            $ss{$pdb_chain}{start} = $pdbResNum;
            		$ss{$pdb_chain}{end} = $pdbResNum;
            		$ss{$pdb_chain}{seq_start} = $p;
            		$ss{$pdb_chain}{seq_end} = $p;
            	 }else{
            		$ss{$pdb_chain}{start} = $pdbResNum if($pdbResNum < $ss{$pdb_chain}{start});
            		$ss{$pdb_chain}{end} = $pdbResNum if($pdbResNum > $ss{$pdb_chain}{end});
            		$ss{$pdb_chain}{seq_start} = $p if($p < $ss{$pdb_chain}{seq_start});
            		$ss{$pdb_chain}{seq_end} = $p if($p > $ss{$pdb_chain}{seq_end});
            	 }
              }
	        }else{
	         #If not then put an X for undef
	         $ss{$pdb_chain}{ssString} .= "X";
	       }
	     }
	   $p++;
     }
   }

    my @ssForMerging;

    #Remove any strings that lack SS
    foreach my $pdb_chain (keys %ss){
      # T,S,B,E,H,G,I
      if($ss{$pdb_chain}{ssString}){
	delete $ss{$pdb_chain} unless($ss{$pdb_chain}{ssString} =~ /[TSBEHGI]/);
      }else{
	delete $ss{$pdb_chain};
      }
      #If we do not delete the hash add it to the 
      if($ss{$pdb_chain}){
	push(@ssForMerging, $ss{$pdb_chain}{ssString});
	my ($pdb, $chain) = split(/_/, $pdb_chain);
	$chain = "" if(!$chain);
	
	#Put the mapping in to the alignment
	my $link = Bio::Annotation::DBLink->new();
	$link->database( "PDB" );
	$link->primary_id( $pdb." ".$chain);
	$link->optional_id( $ss{$pdb_chain}{start}."-".$ss{$pdb_chain}{end}.";"  );
	$seq->annotation(Bio::Annotation::Collection->new()) unless ($seq->annotation);
	$seq->annotation->add_Annotation('dblink', $link);
	$noStructures++;
	#Use this to populate the pdb_pfamA_reg table......!
	my $nse = $seq->acc.".".$seq->version."/".$seq->start."-".$seq->end;
	push(@{$map->{$nse}}, { pdb_id => $pdb,
	                        chain => $chain,
	                        pdb_start => $ss{$pdb_chain}{start},
	                        pdb_end   => $ss{$pdb_chain}{end},
	                        seq_start => $ss{$pdb_chain}{seq_start},
	                        seq_end => $ss{$pdb_chain}{seq_end} } );
	   }
    }

    #Add anything pdbs we have here as an Xref.
    #Compress multiple SS to consenus
    #print Dumper(@ssForMerging);
    if(scalar(@ssForMerging)){
      my $consensus;
      if(scalar(@ssForMerging) > 1){
	       $consensus = secStrucConsensus(\@ssForMerging);
      }else{
	       $consensus = $ssForMerging[0];
      }
      push(@allSsStrings,$consensus);
      $seq->sec_struct( Bio::Pfam::OtherRegion->new('-seq_id' => $seq->acc,
						    '-from' => $start,
						    '-to' => $end,
						    '-type' => "sec_struct",
						    '-display' => $consensus,
						    '-source' => 'Pfam'));
    }
  }

  #Calculate consensus string for the whole alignment.
  if(scalar(@allSsStrings)){
    my $allConsensus;
    if(scalar(@allSsStrings) > 1){
      $allConsensus = secStrucConsensus(\@allSsStrings);
    }else{
      $allConsensus = $allSsStrings[0];
    }
    #print STDERR "**** CON SEC $allConsensus *****\n";
    $aln->cons_sec_struct( Bio::Pfam::OtherRegion->new('-seq_id' => "seq_cons",
						       '-from' => 1,
						       '-to' => length($allConsensus),
						       '-type' => "secondary_structure",
						       '-display' => $allConsensus,
						       '-source' => 'Pfam')); 
  }
  return ($noStructures, $map);
}

sub secStrucConsensus {
  my $ssStringsRef=shift;
  my $num = scalar(@$ssStringsRef);
  my $length=length $$ssStringsRef[0];
  my $gapcount=0;
  my $consensusstring="";
  my (@count,@consensuschar);
  my $numchar=0;
  my ($m,$n,$x,$z,$charindex,$ass,$ambigous,$pos,$character,$prevchar,$gapcharacter,$maxcount);
  $prevchar = "";
  if ($num>1) {
    for ($m=0;$m<$length;$m++) {
      for ($n=0;$n<$num;$n++) {
	$character=substr($$ssStringsRef[$n],$m,1);
	if ($character ne "-" && $character ne "." && $character ne "X") {
	  if (!($prevchar=~/$character/)) {
	    $consensuschar[$numchar]=$character;
	    $count[$numchar]++;
	    $numchar++;
	    $prevchar.=$character;
	  }
	  else {
	    $pos=index $prevchar,$character;
	    $count[$pos]++;
	  }
	}
	else {
	  $gapcount++;
	  $gapcharacter=$character;
	}
      }
      if ($gapcount eq $num) {
	$consensusstring.=$gapcharacter;
      }
      else {
	$maxcount=0;  
	my $end_count=@count;
	for ($x=0;$x<$end_count;$x++){
	  if ($count[$x]>$maxcount) {
	    $charindex=$x;
	    $maxcount=$count[$x];
	  }
	}
	my $ambigious=grep /$maxcount/,@count;
	if ($ambigious>1) {
	  for ($z=0;$z<$end_count;$z++) { # Changed from @count to $end_count for speed
	    if ($count[$z] eq $maxcount) {
	      $ass.=$consensuschar[$z];
	    }
	  }
	  $ass=~s/G/H/g;
	  $ass=~s/I/H/g;
	  $ass=~s/B/E/g;
	  $ass=~s/S/T/g;
	  if ($ass=~/^(.)\1+$/) {
	    $consensusstring.=$1;
	  }
	  else {
	    $consensusstring.="C";
	  }
	  $ass="";
	}
	else {
	  $consensusstring.=$consensuschar[$charindex];
	}
      }
      $prevchar="";
      $gapcount=$numchar=0;
      undef @count;
      undef @consensuschar;
    }
  }
  else {
    $consensusstring=$$ssStringsRef[0];
  }
  return $consensusstring;
}

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;