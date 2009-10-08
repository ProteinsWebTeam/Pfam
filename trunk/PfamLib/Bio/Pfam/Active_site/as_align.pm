package Bio::Pfam::Active_site::as_align;


#This module contains methods that are needed for active site prediction in Pfam, and are not needed for pfam_scan.pl
#It is used in the view process

# For pfama full and seed:
# my $asp = Bio::Pfam::Active_site::as_align->new(-alignment => AlignPfam_obj (full), -auto => $auto_pfamA, -database => PfamLiveDBManager_obj, -seed -> AlignPfam_obj (seed), -nested (optional) => nested_info)
# my $full_aln = $asp->full;
# my $seed_aln = $asp->seed or $asp->seed(AlignPfam_obj of seed) ;

# For pfamb:
# my $asp = Bio::Pfam::Active_site::as_align->new(-alignment => AlignPfam_obj, -auto => $auto_pfamB, -database => PfamLiveDBManager_obj)
# my $pfamb_aln = $asp->pfamB


use strict;
use Bio::SeqFeature::Generic;
use Bio::Pfam::AlignPfam;
use Carp;



=head2 new

 Title    : new
 Usage    : my $asp = new( -alignment => $aln, -auto => "3719");
 Function : constructor for object
 Returns  : Object
 Args     : -alignment => 'Bio::Pfam::AlignPfam object of a pfamA or a pfamB', -auto => "auto_pfamA or auto_pfamB", -database => "PfamLiveDBManager object")
 Optional : -nested => '\@nested' where @nested is an array with elements in the form 'P2P_LACPA/432-553' or 'PfamLive::Nested_locations', -seed => 'Bio::Pfam::AlignPfam object of seed alignment'

=cut

sub new {
  my ( $class, @args ) = @_;

  my $self = {};
  bless ($self, $class);

  # accept both a hash and a hash ref
  my $args = ( ref $args[0] eq 'HASH' ) ? shift @args : { @args };


  croak "FATAL - no alignment passed\n" unless( $args->{-alignment} || $args->{-ALIGNMENT});
  croak "FATAL - no auto number passed\n" unless( $args->{-auto} || $args->{-AUTO});
  croak "FATAL - no database object passed\n" unless( $args->{-database} || $args->{-DATABASE});


  $self->{alignment} = $args->{-alignment} || $args->{-ALIGNMENT};
  $self->{seed} = $args->{-seed} || $args->{-SEED};
  $self->{auto} = $args->{-auto} || $args->{-AUTO};
  $self->{nested} = $args->{-nested} || $args->{-NESTED};
  $self->{database} = $args->{-database} || $args->{-DATABASE};

  return $self;
}


=head2 full

 Title    : full
 Usage    : $asp->full()
 Function : Adds experimental and swiss prot predicted active sites from database
            Predicts active site residues based on experimental active sites
            Uploads pfam predicted active sites to database
            Uploads active site residues and active site alignment to database
 Returns  : Bio::Pfam::AlignPfam object of marked up alignment
 Args     : None

=cut

sub full {
    my ($self) = @_;


    $self->{_expAS} = {};
    $self->{_sprotAS} = {};
    $self->{_pfamAS} = {};


    #Delete old pfam predicted active sites
    my @old_data = $self->{database}->getSchema
	->resultset('PfamseqMarkup')
	->search( { 'pfama_reg_full_significants.auto_pfama' => $self->{auto},
		    auto_markup => '2' },
		  { join    => [ qw ( pfama_reg_full_significants pfamseqs )] });
    

    foreach my $row (@old_data) {
        $row->delete;
    }

    #Delete old active site alignment
    my $old_aln = $self->{database}->getSchema->resultset('ActiveSiteAlignments')->find( {"auto_pfamA" => $self->{auto} });
    if($old_aln) {
	$old_aln->delete;
    }



    

    #Retrieve experimental and swiss prot predicted active sites for sequences in pfamA family
    my @as_data = $self->{database}->getSchema
	->resultset('Pfamseq')
	->search( { auto_pfama => $self->{auto},
		    auto_markup => [qw( 1 3 )],
		    in_full => 1 },
		  { select  => [ qw (pfamseq_acc pfamseq_id pfamseq_markups.residue pfamseq_markups.auto_markup ) ],
		    as      => [qw(pfamseq_acc pfamseq_id residue auto_markup )],
		    join    => [ qw ( pfama_reg_full_significants pfamseq_markups )] });
    
    
    foreach my $as (@as_data){
	if($as->get_column('auto_markup') eq "1") {
	    push(@{$self->{_expAS}->{$as->pfamseq_acc}}, $as->get_column('residue') );
	    push(@{$self->{_expAS}->{$as->pfamseq_id}}, $as->get_column('residue') );
	}
	else {
	    push(@{$self->{_sprotAS}->{$as->pfamseq_acc}}, $as->get_column('residue') );
	    push(@{$self->{_sprotAS}->{$as->pfamseq_id}}, $as->get_column('residue') );
	}
    }

    #Locate exp as in fam
    $self->_locate_as("experimental");


    if($self->{_aln_as}) {
        
	#Print out alignment of active site seq and upload to database
	my $aln_as = $self->{_aln_as}->allgaps_columns_removed();
	open(OUT, ">as_aln.$$") or croak "Can't open full.$$ for writing\n";
	$aln_as->write_Pfam(\*OUT);
	close OUT;
	
	open(GZPP, "gzip -c as_aln.$$ |") or croak "Failed to gzip as_aln.$$:[$!]";
	my $db_as_alignment = join("", <GZPP>);
	close GZPP; 

	$self->{database}->getSchema
	    ->resultset('ActiveSiteAlignments')
	    ->update_or_create( {auto_pfama => $self->{auto},
				 alignment => $db_as_alignment,
				 as_residues => $self->{_as_res_pos} });
	unlink "as_aln.$$";


	#Store as patterns
	$self->_pattern_info();
	
	#Expand exp as
	$self->_expand_as();
    
	#Find pred as
	$self->_add_pred_as();
    }

    #Locate swiss prot predicted as in fam
    $self->_locate_as("sp_pred");

    #Add display line
    $self->_add_display();

    return $self->{alignment};
      
}


=head2 pfamB

 Title    : pfamB
 Usage    : $asp->pfamB($pfamDB)
 Function : Adds experimental and swiss prot predicted active sites from database
            Predicts active site residues based on experimental active sites
 Returns  : Bio::Pfam::AlignPfam object of marked up alignment
 Args     : Bio::Pfam::PfamLiveDBManager object

=cut

sub pfamB {

    my ($self, $pfamDB) = @_;


    $self->{_expAS} = {};
    $self->{_sprotAS} = {};
    $self->{_pfamAS} = {};


    #Delete old pfam predicted active sites
    my @old_data = $self->{database}->getSchema
	->resultset('PfamseqMarkup')
	->search( { 'pfamb_regs.auto_pfamb' => $self->{auto},
		    auto_markup => '2' },
		  { join    => [ qw ( pfamb_regs pfamseqs )] });

    foreach my $row (@old_data) {
        $row->delete;
    }


    #Retrieve experimental and swiss prot predicted active sites for sequences in pfamB family
    my @as_data = $self->{database}->getSchema
	->resultset('Pfamseq')
	->search( { auto_pfamb => $self->{auto},
		    auto_markup => [qw( 1 3 )] },
		  { select  => [ qw (pfamseq_acc pfamseq_id pfamseq_markups.residue pfamseq_markups.auto_markup ) ],
		    as      => [qw(pfamseq_acc pfamseq_id residue auto_markup )],
		    join    => [ qw ( pfamb_regs pfamseq_markups )] });
    
    
    foreach my $as (@as_data){
	if($as->get_column('auto_markup') eq "1") {
	    push(@{$self->{_expAS}->{$as->pfamseq_id}}, $as->get_column('residue') );
	    push(@{$self->{_expAS}->{$as->pfamseq_acc}}, $as->get_column('residue') );
	}
	else {
	    push(@{$self->{_sprotAS}->{$as->pfamseq_id}}, $as->get_column('residue') );
	    push(@{$self->{_sprotAS}->{$as->pfamseq_acc}}, $as->get_column('residue') );
	}
    }	
    
    #Locate exp as in fam
    $self->_locate_as("experimental");
  
    #Store as patterns
    $self->_pattern_info();
    
    #Expand exp as
    $self->_expand_as("experimental");
    
    #Find pred as
    $self->_add_pred_as();
    
    #Locate swiss prot predicted as in fam
    $self->_locate_as("sp_pred");       
    
    #Add display line
    $self->_add_display();
    
    return $self->{alignment};
}


=head2 seed

 Title    : seed 
 Usage    : $asp->seed
 Function : Adds experimental, swiss prot predicted and pfam predicted active sites 
            using data in object
 Returns  : Bio::Pfam::AlignPfam object of marked up alignment
 Args     : None, must add seed alignment to object when calling new

=cut

sub seed {
    my ($self, $seed_aln) = @_;

    if($seed_aln) {
	$self->{seed} = $seed_aln;
    }


    unless($self->{seed}) {
	croak "No seed alignment given\n";
    }

    $self->_mark_seed;
    
    #Add display line
    $self->_add_display("1");

    return $self->{seed};
}


=head2 _locate_as

 Title    : _locate_as
 Usage    : $self->_locate_as($type)
 Function : Adds experimental or swiss prot predicted active site data to alignment object
            If adding experimental data, it also adds an acive site aligment and active site residues to object
 Returns  : Nothing, populates the object with active site residue info
 Args     : type (experimental||sp_pred)
 Optional : $nested_line (P2P_LACPA/432-553)

=cut

sub _locate_as {
 
  my ($self, $type) = @_;

  my $aln_as;

  my $as_res;
  if($type eq "experimental") {
      $as_res = $self->{_expAS};
      $aln_as = new Bio::Pfam::AlignPfam;
  }
  elsif($type eq "sp_pred") {
      $as_res = $self->{_sprotAS}
  }
  else {
      croak "Incorrect argument passed\n";
  }

  my (@nested_starts, @nested_ends);  


  if ($self->{nested}) {
     
      foreach my $line (@{$self->{nested}}) {
	  
	  my ($nested_id, $nested_start, $nested_end);
	  
	  if ($line =~ /(\S+)\/(\d+)\-(\d+)/) {  #P2P_LACPA/432-553
	      $nested_id = $1;
	      $nested_start = $2;
	      $nested_end = $3;	
	  }
	  elsif(ref($line) eq "PfamLive::Nested_locations"){
	      $nested_id = $line->pfamseq_acc;
	      $nested_start = $line->seq_start;
	      $nested_end = $line->seq_end;

	  } 
	  else {
	      croak "Don't understand nested format [$line]\n";
	  }
	  
	  #convert nested start/end to column positions
	  $nested_start = $self->{alignment}->column_from_residue_number($nested_id, $nested_start); 
	  $nested_end = $self->{alignment}->column_from_residue_number($nested_id, $nested_end); 
	  
	  push(@nested_starts, $nested_start);
	  push(@nested_ends, $nested_end);
      }
      
  } 


  my $flag2;
  foreach my $seq ($self->{alignment}->each_seq) {
      my %already;
      my $flag;
      foreach my $pos ( @{$as_res->{$seq->id}}) {

	  if($pos >= $seq->start and $pos <= $seq->end) { #Feature is in the alignment
	      
	      #store column position for seq
	      my $col = $self->{alignment}->column_from_residue_number($seq->id, $pos);              
	      next if(exists($already{$col}));
	      
	      #ignore anything in the nested region     
	      my $ignore;
	      for(my $i=0; $i<@nested_starts; $i++) {
		  if( ($col >= $nested_starts[$i]) and ($col <= $nested_ends[$i]) ) {                                     
		      $ignore=1;
                      last;
		  }
	      } 
	      next if($ignore);
	      
	      
	      #Don't add swiss prot active sites if its already been predicted by pfam
	      if($type eq "sp_pred") {
		  if($seq->feature_count()) {
		      foreach my $feat ($seq->all_SeqFeatures()) {
			  $already{$feat->start}=1;
		      }
		  }
		  next if(exists($already{$col}));
	      }
	      
	      
	      #add feature to seq
	      my $aa .= uc substr($seq->seq(), $col-1, 1); 
	      
	      my $feat = new Bio::SeqFeature::Generic  (  -display_name => $type,
							  -primary => $aa,
							  -start => $col);
	      
	      
	      $seq->add_SeqFeature($feat);
              if($type eq "experimental") {
		  $self->{_as_res_pos} .= $seq->id . "  $pos, " ;
		  $flag=1;

	      }

	  }
	  
      }
      if($flag) {
	  $flag2=1;
	  $aln_as->add_seq($seq); 
      }
  
  }

  if($flag2) {
      $self->{_aln_as} = $aln_as;
  } 


}


=head2 _pattern_info

 Title    : _pattern_info
 Usage    : $self->_pattern_info
 Function : Collects active site patterns into an alignment
 Returns  : Nothing, adds pattern_aln to object
 Args     : None

=cut

sub _pattern_info {

    my ($self) = @_;

    my $pattern_aln = new Bio::Pfam::AlignPfam;

    my (%pat_col_seq);
  
    foreach my $seq ( $self->{alignment}->each_seq() ) {  

	next unless($seq->all_SeqFeatures());
           my ($pat, $col);
           foreach my $feat ( sort {$a->start <=> $b->start }  $seq->all_SeqFeatures() ) {            
              $pat .= $feat->primary_tag();   #HEK
              $col .= $feat->start() . " ";    #33 44 55
	   }

           unless(exists($pat_col_seq{"$pat:$col"})) {
	       $pattern_aln->add_seq($seq); 
               $pat_col_seq{"$pat:$col"}=1;
	   }
    }

    $self->{_pattern_aln} = $pattern_aln;
    
}


=head2 _add_pred_as

 Title    : _add_pred_as
 Usage    : $self->_add_pred_as
 Function : Predicts active sites based on known active site data
 Returns  : Nothing, adds predicted active sites to object
 Args     : None

=cut

sub _add_pred_as {

    my ($self) = @_;
    my $num_seq=0;


    foreach my $seq1 ( $self->{alignment}->each_seq() ) {  

       next if($seq1->feature_count());

       my $aligns_with = new Bio::Pfam::AlignPfam; 

       
       foreach my $seq2  ( $self->{_pattern_aln}->each_seq() ) {  
                       
         
           #See if all active site residues from seq2 exist in seq1
           my $mismatch;
           foreach my $feat ( sort {$a->start <=> $b->start }  $seq2->all_SeqFeatures() ) {           
   
              my $aa1 = $feat->primary_tag();
              my $col = $feat->start();
               
              my $aa2 = uc substr($seq1->seq, $col-1, 1); 
              unless($aa1 eq $aa2) {
	          $mismatch = 1;
                  last;	 
          
              }
  
           }              
         
           #Store seq2 if all active site residues are present in seq1
	   unless($mismatch) {
              $aligns_with->add_seq($seq2);
	   }
       }


       
       $num_seq = $aligns_with->no_sequences();
       next unless($num_seq);   
       my (%seq_to_remove, %seq_to_rem);  #two hashes used to collect seq that need removing


        #if seq1 matches more than one pattern remove subpatterns and any patterns that overlap

        #first remove sub pat
        if($num_seq>1) { 
           foreach my $sequence1 ($aligns_with->each_seq() ) {    
              foreach my $sequence2 ($aligns_with->each_seq() ) {

                   next if($sequence1 eq $sequence2);    
     
                   my (%hash1, %hash2, $num_1, $num_2, %smaller, %larger);
                   #collect column positions
                   foreach my $feat1 ($sequence1->all_SeqFeatures() ) { 
                       $hash1{$feat1->start} =1;
                       $num_1++;
		   }
                   foreach my $feat2 ($sequence2->all_SeqFeatures() ) { 
                       $hash2{$feat2->start} =1;
                       $num_2++;
		   }

		   
                   #see if one is a subpattern of the other
                   my $diff=0; 
                   unless($num_1 eq $num_2) {
		       
                       my $remove_seq;

                       if($num_1 > $num_2) {
			   %smaller = %hash2;
                           %larger = %hash1;
                           $remove_seq = $sequence2;
                
		       }
                       else {
			   %smaller = %hash1;
                           %larger = %hash2;
                           $remove_seq = $sequence1;
		       }

                       
                       foreach my $key (keys %smaller) {
			   $diff = 1 unless(exists($larger{$key}));  #diff is true if it is not a subpattern
		       }
                      

                       $seq_to_rem{$remove_seq}= $remove_seq unless($diff) ;  
                       next unless($diff);    
		   } 
             }

           }
         }
         
         #Now remove any patterns which need removing
         foreach my $remove (keys %seq_to_rem) {
           $aligns_with->remove_seq($seq_to_rem{$remove});
         }


         unless($num_seq >=1) {
            croak "All sequences that align with seq have been removed - this shouldn;t happen\n";
         } 



        $num_seq = $aligns_with->no_sequences();
        #and then any patterns that overlap
        if($num_seq>1) { 

           foreach my $sequence1 ($aligns_with->each_seq() ) {    
	       
              foreach my $sequence2 ($aligns_with->each_seq() ) {
                   next if($sequence1 eq $sequence2);    
     
                   my ($seq1_st, $seq1_en, $seq2_st, $seq2_en);

                   my (%hash1, %hash2, $num_1, $num_2, %smaller, %larger);

                   #see if patterns overlap - find pattern start ends and collect column positions
                   foreach my $feat1 ($sequence1->all_SeqFeatures() ) { 

		       $seq1_st = $feat1->start() if(!$seq1_st or $feat1->start() < $seq1_st);
                       $seq1_en = $feat1->start() if(!$seq1_en or $feat1->start() > $seq1_en);
		   }

                   foreach my $feat2 ($sequence2->all_SeqFeatures() ) { 

		       $seq2_st = $feat2->start() if(!$seq2_st or $feat2->start() < $seq2_st);
                       $seq2_en = $feat2->start() if(!$seq2_en or $feat2->start() > $seq2_en);
		   }
                 		                     
                   #then see if patterns overlap - remove sequence with pattern of least identity
                   if(($seq1_st >= $seq2_st and $seq1_st <= $seq2_en) or ($seq2_st >= $seq1_st and $seq2_st <= $seq1_en)) {                       
		       my $remove = _identity($seq1, $sequence1, $sequence2);
                       $seq_to_remove{$remove}= $remove;
                   }
             }

           }
       }
         
       #Now remove any patterns which need removing
       foreach my $remove (keys %seq_to_remove) {
           $aligns_with->remove_seq($seq_to_remove{$remove});
           $num_seq = $aligns_with->no_sequences();
           last if($num_seq eq "1"); #just in case the % identities are identical
       }

         
       $num_seq = $aligns_with->no_sequences();
       unless($num_seq >=1) {
	   croak "All sequences that align with seq have been removed - this shouldn't happen\n";
       } 

       #Add features to seq
       foreach my $sequence ($aligns_with->each_seq() ) {             
	   foreach my $feat ($sequence->all_SeqFeatures() ) {     
	       
	       my $actual_pos = $seq1->location_from_column($feat->start); 
	       $actual_pos = $actual_pos->start();
	       
	       my $pred_feat = new Bio::SeqFeature::Generic  (  -display_name => 'pfam_pred',
								-primary => $feat->primary_tag,
								-start => $feat->start,
								-tag => { position => $actual_pos} );

	       

	       $seq1->add_SeqFeature($pred_feat);  
	       push(@{$self->{_pfamAS}->{$seq1->id}}, $actual_pos);

	       my $auto_pfamseq = $self->id2acc($seq1->id);

	       $self->{database}->getSchema
		   ->resultset('PfamseqMarkup')
		   ->update_or_create( {auto_pfamseq => $auto_pfamseq,
					auto_markup => "2",
					residue => $actual_pos });

	   }
       }
        
       
   }
}


=head2 _expand_as

 Title    : _expand_as
 Usage    : $self->_expand_as()
 Function : Adds predicted active sites to sequences with experimental active sites
 Returns  : Nothing, adds predicted active sites to object
 Args     : None

=cut

sub _expand_as {
    my ($self) = @_;

    foreach my $seq1 ( $self->{alignment}->each_seq() ) { 
          	
          next unless($seq1->feature_count());

          foreach my $seq2 ($self->{alignment}->each_seq()) {
              next unless($seq2->feature_count());

              #Don't compare a seq to itself
              next if($seq1 eq $seq2);

              my $mismatch;	      
              my (%hash1, %hash2, $expand);
              my $st =0;
              my $en =0;

              #Make hash of all act_site col positions for both seq
              foreach my $feat1 ( $seq1->all_SeqFeatures() ) {
		  $hash1{$feat1->start}=1;                 
	      }
              foreach my $feat2 ($seq2->all_SeqFeatures() ) {
		  $hash2{$feat2->start}=1;
                  $expand =1 if($feat2->display_name eq 'expand');
	      }
              next if($expand);  #ignore expanded pattern - best find a sequence with all the experimental active sites

              #check all act sites in seq1 exist in seq2
              foreach my $col1 (keys %hash1) {      
		  if(exists($hash2{$col1})) {

                     my $aa1 .= uc substr($seq1->seq, $col1-1, 1);  
                     my $aa2 .= uc substr($seq2->seq, $col1-1, 1);
                     unless($aa1 eq $aa2) {
			 $mismatch=1;
                         last;
		     }
		  }
                  else {
		      $mismatch = 1;    #mismatch means seq2 doesn't contain all the as from seq1
                      last;
		  }
		   
	      }



              #Mark additional act site if act site in seq2 exists in seq1
              unless($mismatch) {  #only mark if seq2 has same as seq1

                foreach my $col2 (keys %hash2) {
		  unless(exists($hash1{$col2})) {
		     
                     my $aa1 .= uc substr($seq1->seq, $col2-1, 1);  #key2 is column position from $seq2
                     my $aa2 .= uc substr($seq2->seq, $col2-1, 1);  
                     if($aa1 eq $aa2) {

                        my $actual_pos = $seq1->location_from_column($col2); 
                        $actual_pos = $actual_pos->start(); 
                        my $feat = new Bio::SeqFeature::Generic  (  -display_name => 'expand',
                                                                    -primary => $aa1,
							            -start => $col2,
                                                                    -tag => { position => $actual_pos} );

                        $seq1->add_SeqFeature($feat);                      
			my $auto_pfamseq = $self->id2acc($seq1->id);

			$self->{database}->getSchema
			    ->resultset('PfamseqMarkup')
			    ->update_or_create( {auto_pfamseq => $auto_pfamseq,
						 auto_markup => "2",
						 residue => $actual_pos });
                       
		    } 
	         }
               }      
             
	     }    
          }
  
    }
}


=head2 _mark_seed

 Title    : _mark_seed
 Usage    : $self->_mark_seed
 Function : Adds experimental, swiss prot predicted and pfam predicted active sites based on data in the full alignment
 Returns  : Nothing, add active sites to seed alignment in object
 Args     : None

=cut

sub _mark_seed {

    my ($self) = @_;

      foreach my $seq ( $self->{seed}->each_seq() ) { 	

	 if(exists($self->{_expAS}->{$seq->id})) {
	     foreach my $pos (@{$self->{_expAS}->{$seq->id}}) {
		 if($pos >= $seq->start and $pos <= $seq->end) {
                       my $col = $self->{seed}->column_from_residue_number($seq->id, $pos); #get column position
                       my $aa .= uc substr($seq->seq(), $col-1, 1);   

                       my $feat = new Bio::SeqFeature::Generic  (  -display_name => 'experimental',
                                                                   -primary => $aa,
							           -start => $col);
                       $seq->add_SeqFeature($feat);
	          }
	     }
	 }


          
	 if(exists($self->{_pfamAS}->{$seq->id})) {
	     foreach my $pos (@{$self->{_pfamAS}->{$seq->id}}) {  
		 if($pos >= $seq->start and $pos <= $seq->end) {
                      my $col = $self->{seed}->column_from_residue_number($seq->id, $pos); #get column position
                       my $aa .= uc substr($seq->seq(), $col-1, 1);   

                       my $feat = new Bio::SeqFeature::Generic  (  -display_name => 'pfam_pred',
                                                                   -primary => $aa,
		     					           -start => $col);

                       $seq->add_SeqFeature($feat);
	          }
	     }
	 }

	 my %already;
         if(exists($self->{_sprotAS}->{$seq->id})) {
	     foreach my $pos (@{$self->{_sprotAS}->{$seq->id}}) {
		 if($pos >= $seq->start and $pos <= $seq->end) {

		     my $col = $self->{seed}->column_from_residue_number($seq->id, $pos); #get column position

                     #Don't mark if already have a pfam predicted site 
		     if($seq->feature_count()) {
			 foreach my $feat ($seq->all_SeqFeatures()) {
			     $already{$feat->start}=1;
			 }
		     }
		     next if(exists($already{$col}));

		     my $aa .= uc substr($seq->seq(), $col-1, 1);   

		     my $feat = new Bio::SeqFeature::Generic  (  -display_name => 'sp_pred',
								 -primary => $aa,
								 -start => $col);
		     $seq->add_SeqFeature($feat);
	         }
	     }
	 }
     }
}


=head2 _add_display

 Title    : _add_display
 Usage    : $self->_add_display
 Function : Adds 'display line' (e.g. .....*....) to show where active site residues are located
 Returns  : Nothing, adds Bio::Pfam::OtherRegion to sequences
 Args     : If no arguments are given, $self->{alignment} is the input alignment
            If argument is given, $self->{seed}  is the input alignment

=cut

sub _add_display {
    my ($self, $seed) = @_;

    my $aln;
    if($seed) {
	$aln = $self->{seed};
    }
    else {
	$aln = $self->{alignment};
    }

    foreach my $seq ( $aln->each_seq() ) { 

      next unless($seq->feature_count());

      my $AS_string = "." x length($seq->seq);
      my $pAS_string = $AS_string;
      my $sAS_string = $AS_string;


      my ($AS, $pAS, $sAS);
      foreach my $feat ( $seq->all_SeqFeatures() ) {
	  if($feat->display_name eq "experimental") {
              substr($AS_string, $feat->start()-1, 1, "*");
              $AS = 1;
	  }
          elsif($feat->display_name eq "expand") {
              substr($pAS_string, $feat->start()-1, 1, "*");
              $pAS =1;
	  }
          elsif($feat->display_name eq "pfam_pred") {
              substr($pAS_string, $feat->start()-1, 1, "*");
              $pAS =1;
	  }
          elsif($feat->display_name eq "sp_pred") {
              substr($sAS_string, $feat->start()-1, 1, "*");
              $sAS =1;
	  }      
      }

      if($AS) {
           my $asObj = Bio::Pfam::OtherRegion->new('-seq_id' => $seq->id(),
                                    '-from'   => 1,
                                    '-to'     => length($seq->seq),
                                    '-type'   => "active_site",
                                    '-display'=> $AS_string,
                                    '-source' => 'Pfam');
           $seq->active_site($asObj);
      }
      if($pAS) {
           my $asObj = Bio::Pfam::OtherRegion->new('-seq_id' => $seq->id(),
                                    '-from'   => 1,
                                    '-to'     => length($seq->seq),
                                    '-type'   => "pfam_pred_active_site",
                                    '-display'=> $pAS_string,
                                    '-source' => 'Pfam');
           $seq->pfam_pred_active_site($asObj);
      }
      if($sAS) {
           my $asObj = Bio::Pfam::OtherRegion->new('-seq_id' => $seq->id(),
                                    '-from'   => 1,
                                    '-to'     => length($seq->seq),
                                    '-type'   => "sprot_pred_active_site",
                                    '-display'=> $sAS_string,
                                    '-source' => 'Pfam');
           $seq->sprot_pred_active_site($asObj);
      }

  }
}


sub id2acc {
  my ( $self, $acc ) = @_;
  
  my $result = $self->{database}->getSchema->resultset("Pfamseq")->find( { "pfamseq_acc" => $acc } );

  unless($result) {
      $result =$self->{database}->getSchema->resultset("Pfamseq")->find( { "pfamseq_id" => $acc } );
  }

  return ( $result->auto_pfamseq );

}



sub _identity {
  my $seq1 = shift;
  my @aligns_with = @_;
  my $lower_identity=100;
  my $lower_identity_seq;

  foreach my $s (@aligns_with) {
    my $tmp_aln = new Bio::Pfam::AlignPfam;
    $tmp_aln->add_seq($s);
    $tmp_aln->add_seq($seq1);
    my $identity = $tmp_aln->overall_percentage_identity();
    if($identity < $lower_identity) {
                  $lower_identity = $identity;
      $lower_identity_seq = $s;
          }
  }
  return $lower_identity_seq;
}



1;


