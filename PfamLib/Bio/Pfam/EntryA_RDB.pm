
#
# BioPerl module for Bio::Pfam::EntryA_RDB
#
# Cared for by Kevin Howe <pfam@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::EntryA_RDB - DESCRIPTION

Concrete implementation of a Pfam-A entry. This particular implementation
is for those entries that 'exist' in a Pfam RDB

=head1 SYNOPSIS

The user shoud not instantiate this object directly, but will tranparently
receive t via a call to DB::get_EntryA_by_acc (or id)

=head1 DESCRIPTION

Pfam RDBs only contain a limited set of the information normally found 
in a Pfam entry. Therefore, many of the methods throw exceptions. Also,
The data is retrieved in a completely different way than other 
Entry implementation. Therefore core functionality in the the superclasses
for parsing entries, is not used.

=head1 CONTACT

pfam@sanger.ac.uk

=cut


# Let the code begin...

# Author: rdf

package Bio::Pfam::EntryA_RDB;
use vars qw($AUTOLOAD @ISA);
use strict;
use warnings;

use Bio::Pfam::AlignPfam;
use Bio::Pfam::EntryA;

@ISA = qw(Bio::Pfam::EntryA);


sub new {
  my($class, %params) = @_;

  my ($the_rdb) = ($params{'-RDB'}||$params{'-rdb'});

  my $self = $class->SUPER::new(%params);
  $self->_the_RDB( $the_rdb );

  return $self;
}



=head2 add_build_line

 Title   : add_build_line
 Usage   : $self->add_build_line($line)
 Function: adds another build line to the object
 Example :
 Returns : 
 Args    :


=cut

sub add_build_line{
   my ($self,$line) = @_;

   $self->throw("Bio::Pfam::EntryA_RDB - method add_build_line is not implemented");

}

=head2 each_build_line

 Title   : each_build_line
 Usage   : gives you an array of build lines
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub each_build_line{
   my ($self,@args) = @_;

   $self->throw("Bio::Pfam::EntryA_RDB - method each_build_line is not implemented");
}


=head2 entry_type

 Title   : entry_type
 Usage   : $self->entry_type($len)
 Function: gets/sets the length of the HMM model (match states) for this pfam familiy
 Returns : 
 Args    :


=cut

sub entry_type {
   my ($self,$value) = @_;
 
   if( defined $value) {
       $self->{'entry_type'} = $value;
   }
   
   return $self->{'entry_type'};


  
   
   return $self->{'entry_type'};
}


=head2 add_forward_acc

 Title   : add_forward_acc
 Usage   : $self->add_forward_acc($line)
 Function: adds another forwarding acc to the object
 Example :
 Returns : 
 Args    :


=cut

sub add_forward_acc {
   my ($self,$acc) = @_;

   $self->throw("Bio::Pfam::EntryA_RDB - method add_forward_acc is not implemented");
}


=head2 each_forward_acc

 Title   : each_forward_acc
 Usage   : gives you an array of forwarding accessions
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub each_forward_acc {
   my ($self,@args) = @_;

   $self->throw("Bio::Pfam::EntryA_RDB - method each_forward_acc is not implemented");
}



=head2 acc

 Title   : acc
 Usage   : $self->acc($newval)
 Function: 
 Example : 
 Returns : value of acc
 Args    : newvalue (optional)


=cut

sub acc{
    my ($self,$value) = @_;

    if( defined $value) {
	$self->{'acc'} = $value;
    }
    
    return $self->{'acc'};    
}


sub auto_pfamA{
    my ($self,$value) = @_;

      if( defined $value) {
	$self->{'auto_pfamA'} = $value;
    }
    
    return $self->{'auto_pfamA'}; 
    
}

=head2 annotated_regions

 Title   : 
 Usage   : @regs = $self->pfam_regions
 Function:
 Example :
 Returns : A list of AnnotatedRegion for the entry
 Notes   : 
    The regions returned will have undefined fields for the
    hmm start-ends. This information is not available in the RDB

=cut

sub all_full_annotated_regions {
   my ($self,$type) = @_;
   my ($seqid, $seqacc, $seqdesc, $st, $en, $mo_st, $mo_en, $bits, $ev, $stat, @list, $mode, $sig, $in_full, $seq_bits, $seq_eval );

   my $auto_pfamA = $self->auto_pfamA();
  
   eval {
       my $dbh = $self->_the_RDB->connect();

       $stat = "select pfamseq_id, pfamseq_acc, description, seq_start, seq_end, model_start, model_end, domain_bits_score, domain_evalue_score, sequence_bits_score, sequence_evalue_score, mode from pfamA_reg_full, pfamseq where auto_pfamA = '$auto_pfamA' and pfamseq.auto_pfamseq = pfamA_reg_full.auto_pfamseq";
       
       my $handle = $dbh->prepare($stat);
       $handle->execute();

       while ( ($seqid, $seqacc, $seqdesc, $st, $en, $mo_st, $mo_en, $bits, $ev, $seq_bits, $seq_eval, $mode )  = $handle->fetchrow) {
	   push @list,  Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION'  => $self->acc(),
						   '-PFAM_ID'         => $self->id(),
						   '-SEQ_ID'          => $seqid,
						   '-SEQ_ACC'         => $seqacc,
						   '-SEQ_DESC'        => $seqdesc,
						   '-FROM'            => $st,
						   '-TO'              => $en,
						   '-MODEL_FROM'      => $mo_st,
						   '-MODEL_TO'        => $mo_en,
						   '-BITS'            => $bits,
						   '-EVALUE'          => $ev,
						   '-ANNOTATION'      => $self->ann,
						   '-MODE'            => $mode,
						   '-SEQUENCE_BITS'   => $seq_bits,
						   '-SEQUENCE_EVALUE' => $seq_eval,
						   );
       }
       $handle->finish;
   };
   $@ and $self->throw("Bio::Pfam::EntryA_RDB->annotated_regions - RDB error $@");

   return @list;
}


=head2 annotated_regions

 Title   : 
 Usage   : @regs = $self->pfam_regions
 Function:
 Example :
 Returns : A list of AnnotatedRegion for the entry
 Notes   : 
    The regions returned will have undefined fields for the
    hmm start-ends. This information is not available in the RDB

=cut

sub annotated_regions {
   my ($self,$type) = @_;
   my ($seqid, $st, $en, $mo_st, $mo_en, $bits, $ev, $stat, @list, $mode, $sig, $in_full, $seq_bits, $seq_eval );


 
   my $auto_pfamA = $self->auto_pfamA();
  
   eval {
       my $dbh = $self->_the_RDB->connect();

       if ($type eq 'FULL') {

	   $stat = "select auto_pfamseq, seq_start, seq_end, model_start, model_end, ";
	   $stat .= "domain_bits_score, domain_evalue_score, sequence_bits_score , sequence_evalue_score, mode, significant , in_full  from pfamA_reg_full where auto_pfamA = '$auto_pfamA'";
	   
       }
       else {
	
	   $stat = "select auto_pfamseq, seq_start, seq_end from pfamA_reg_seed where auto_pfamA = '$auto_pfamA'";
	  
       }

       my $handle = $dbh->prepare($stat);
       $handle->execute();

       if ($type eq 'FULL') {
	   while ( ($seqid, $st, $en, $mo_st, $mo_en, $bits, $ev, $seq_bits, $seq_eval, $mode, $sig, $in_full)  = $handle->fetchrow) {
	 push @list,  Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $self->acc(),
						 '-PFAM_ID' => $self->id(),
						 '-PFAMSEQ_ID' => $seqid,
						 '-FROM' => $st,
						 '-TO' => $en,
						 '-MODEL_FROM' => $mo_st,
						 '-MODEL_TO' => $mo_en,
						 '-BITS' => $bits,
						 '-EVALUE' => $ev,
						 '-DOMAIN_BITS' => $bits,
						 '-DOMAIN_EVALUE' => $ev,
						 '-ANNOTATION' => $self->ann,
						 '-MODE' => $mode,
						 '-SEQUENCE_BITS' =>  $seq_bits,
						 '-SEQUENCE_EVALUE' =>  $seq_eval,
						 '-IS_SIGNIFICANT' => $sig,
						 '-IN_FULL' =>$in_full
						);
	     }
	   $handle->finish;
     }       else {
	   while ( ($seqid, $st, $en )
		   = $handle->fetchrow) {
	       push @list,  Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $self->acc(),
						       '-PFAM_ID' => $self->id(),
						       '-SEQ_ID' => $seqid,
						       '-FROM' => $st,
						       '-TO' => $en,
						       '-ANNOTATION' => $self->ann);
	   }
	   $handle->finish;
       }

   };
   $@ and $self->throw("Bio::Pfam::EntryA_RDB->annotated_regions - RDB error $@");

   return @list;
}


=head2 id

 Title   : id
 Usage   : $self->id($newval)
 Function: 
 Example : 
 Returns : value of id
 Args    : newvalue (optional)

=cut

sub id{
   my ($self,$value) = @_;
   
   if( defined $value) {
       $self->{'id'} = $value;
   }
   
   return $self->{'id'};
   
}



=head2 author

 Title   : author
 Usage   : $self->author($newval)
 Function: 
 Example : 
 Returns : value of author
 Args    : newvalue (optional)


=cut

sub author{
   my ($self,$value) = @_;

    if( defined $value) {
       $self->{'author'} = $value;
   }
   
   return $self->{'author'};

 }


=head2 model_length

 Title   : model_length
 Usage   : $self->id($newval)
 Function: 
 Example : 
 Returns : value of id
 Args    : newvalue (optional)

=cut

sub model_length{
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'model_length'} = $value;
   }
   
   return $self->{'model_length'};
}



=head2 source

 Title   : source
 Usage   : $self->source($newval)
 Function: 
 Example : 
 Returns : value of source
 Args    : newvalue (optional)


=cut

sub source{
   my ($self,$value) = @_;

     if( defined $value) {
       $self->{'source'} = $value;
   }
   
   return $self->{'source'};
}





sub ls_kappa{
   my ($self,$value) = @_;


      if( defined $value) {
	$self->{'ls_kappa'} = $value;
    }

 
    
   return $self->{'ls_kappa'};

}

sub ls_mu{
   my ($self,$value) = @_;

      if( defined $value) {
	$self->{'ls_mu'} = $value;
    }

    
   return $self->{'ls_mu'};

}


sub fs_kappa{
   my ($self,$value) = @_;

      if( defined $value) {
	$self->{'fs_kappa'} = $value;
    }


   return $self->{'fs_kappa'};

}

sub fs_mu{
   my ($self,$value) = @_;

      if( defined $value) {
	$self->{'fs_mu'} = $value;
    }
    
   return $self->{'fs_mu'};

}




=head2 alignmethod

 Title   : alignmethod
 Usage   : $self->alignmethod($newval)
 Function: 
 Example : 
 Returns : value of alignmethod
 Args    : newvalue (optional)


=cut

sub alignmethod{
   my ($self,$value) = @_;
  if( defined $value) {
       $self->{'alignmethod'} = $value;
   }
   
   return $self->{'alignmethod'};
  
}



=head2 gathering_cutoff

 Title   : gathering_cutoff
 Usage   : $self->gathering_cutoff($newval)
 Function: 
 Example : 
 Returns : value of gathering cutoff
 Args    : newvalue (optional)


=cut

sub gathering_cutoff {
   my ($self,$value) = @_;

   $self->throw("Bio::Pfam::EntryA_RDB - gathering_cutoff is not implemented");
}



##################
# NEW GATHERING PARAMETERS
######

sub ls_sequence_gathering_cutoff {
   my ($self,$value) = @_;

      if( defined $value) {
	$self->{'ls_sequence_gathering'} = $value;
    }



   return $self->{'ls_sequence_gathering'};
}


sub ls_domain_gathering_cutoff {
   my ($self,$value) = @_;


      if( defined $value) {
	$self->{'ls_domain_gathering'} = $value;
    }


   return $self->{'ls_domain_gathering'};
}

sub fs_sequence_gathering_cutoff {
   my ($self,$value) = @_;


      if( defined $value) {
	$self->{'fs_sequence_gathering'} = $value;
    }


   return $self->{'fs_sequence_gathering'};

}


sub fs_domain_gathering_cutoff {
   my ($self,$value) = @_;

      if( defined $value) {
	$self->{'fs_domain_gathering'} = $value;
    }

   return $self->{'fs_domain_gathering'};

}

#/ end new GA PARAMS



=head2 trusted_cutoff

 Title   : trusted_cutoff
 Usage   : $self->trusted_cutoff($newval)
 Function: 
 Example : 
 Returns : value of trusted_cutoff
 Args    : newvalue (optional)


=cut

sub trusted_cutoff{
   my ($self,$value) = @_;

   $self->throw("Bio::Pfam::EntryA_RDB - trusted_cutoff is not implemented");
}





##################
# NEW TRUSTED PARAMETERS
######

sub ls_sequence_trusted_cutoff {
   my ($self,$value) = @_;

    if( defined $value) {
	$self->{'fs_domain_gathering'} = $value;
    }


   return $self->{'ls_sequence_trusted'};
}


sub ls_domain_trusted_cutoff {
   my ($self,$value) = @_;


    if( defined $value) {
	$self->{'fs_domain_gathering'} = $value;
    }

   return $self->{'ls_domain_trusted'};
}

sub fs_sequence_trusted_cutoff {
   my ($self,$value) = @_;

    if( defined $value) {
	$self->{'fs_domain_gathering'} = $value;
    }

   return $self->{'fs_sequence_trusted'};

}


sub fs_domain_trusted_cutoff {
   my ($self,$value) = @_;

    if( defined $value) {
	$self->{'fs_domain_gathering'} = $value;
    }


   return $self->{'fs_domain_trusted'};

}

#/ end new TRUSTED PARAMS


=head2 noise_cutoff

 Title   : noise_cutoff
 Usage   : $self->noise_cutoff($newval)
 Function: 
 Example : 
 Returns : value of noise_cutoff
 Args    : newvalue (optional)


=cut

sub noise_cutoff{
   my ($self,$value) = @_;

   $self->throw("Bio::Pfam::EntryA_RDB - noise_cutoff is not implemented");
}




##################
# NEW NOISE PARAMETERS
######

sub ls_sequence_noise_cutoff {
   my ($self,$value) = @_;

 if( defined $value) {
	$self->{'ls_sequence_noise'} = $value;
    }


   return $self->{'ls_sequence_noise'};
}

sub ls_domain_noise_cutoff {
   my ($self,$value) = @_;
 if( defined $value) {
	$self->{'ls_domain_noise'} = $value;
    }


   return $self->{'ls_domain_noise'};
}

sub fs_sequence_noise_cutoff {
   my ($self,$value) = @_;

 if( defined $value) {
	$self->{'fs_sequence_noise'} = $value;
    }

   return $self->{'fs_sequence_noise'};

}


sub fs_domain_noise_cutoff {
   my ($self,$value) = @_;

 if( defined $value) {
	$self->{'fs_domain_noise'} = $value;
    }

 
   return $self->{'fs_domain_noise'};

}

#/ end new NOISE PARAMS


=head2 previous_ids

 Title   : previous_ids
 Usage   : $obj->previous_ids($newval)
 Function: 
 Example : 
 Returns : value of previous_ids
 Args    : newvalue (optional)


=cut

sub previous_ids{
    my ($self,$value) = @_;
    
  if( defined $value) {
	$self->{'previous_ids'} = $value;
    }
return $self->{'previous_ids'};
}



=head2 is_dead

 Title   : is_dead
    Usage   : if ($en->is_dead()) { ...
 Function:
 Example :
 Returns : Boolean
 Args    :


=cut

sub is_dead {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'is_dead'} = $value;
   }
   return $self->{'is_dead'};

}
  



=head2 num_seqs_in_full

 Title   : 
 Usage   : $count = $self->num_seqs_in_seed
 Function:
 Example :
 Returns : The number of sequences in the full alignment for the entry
 Args    : The number of sequences in the full alignment for the entry (optional)


=cut

sub num_seqs_in_full {
   my ($self, $value) = @_;

   if( defined $value) {
       $self->{'num_seqs_in_full'} = $value;
   }
   
   return $self->{'num_seqs_in_full'};
}



sub comment{
   my ($self,$value) = @_;
 if( defined $value) {
       $self->{'comment'} = $value;
   }

  
    
   return $self->{'comment'};

}

=head2 num_seqs_in_seed

 Title   : 
 Usage   : $count = $self->num_seqs_in_seed
 Function:
 Example :
 Returns : The number of sequences in the seed alignment for the entry
 Args    : The number of sequences in the seed alignment for the entry (optional)


=cut

sub num_seqs_in_seed {
   my ($self, $value) = @_;

   if( defined $value) {
       $self->{'num_seqs_in_seed'} = $value;
   }
   
   return $self->{'num_seqs_in_seed'};
}


=head2 seed

 Title   : seed
 Usage   : $seed = $self->seed()
 Function:
 Example :
 Returns : Bio::Pfam::AlignPfam object of the seed alignment
 Args    :


=cut

sub seed{
   my ($self,@args) = @_;

   $self->throw("Bio::Pfam::EntryA_RDB - seed is not implemented");					    
}


=head2 full

 Title   : full
 Usage   : $full = $self->full()
 Function:
 Example :
 Returns : Bio::Pfam::AlignPfam object of the full alignment
 Args    :


=cut

sub full{
   my ($self,@args) = @_;

   $self->throw("Bio::Pfam::EntryA_RDB - full is not implemented");					    
}


=head2 ann

 Title   : ann
 Usage   : $obj->ann($newval)
 Function: 
 Example : 
 Returns : value of ann
 Args    : newvalue (optional)


=cut

sub ann{
    my ($obj,$value) = @_;

    if( $value) {
	$obj->{'_web_ann'} = $value;
    }

    return $obj->{'_web_ann'};
}



=head2 name_start_end

 Title   : 
 Usage   : @names = $self->name_start_end('FULL')
 Function:
 Example :
 Returns : a list of "name/start-end" for the alignment of choice 
 Args    :


=cut

sub name_start_end {
   my ($self,$type) = @_;
   my ($seqid, $st, $en,  @list);

   my $table = ($type eq 'FULL')?'pfamA_reg_full':'pfamA_reg_seed';

   eval {
       my $dbh = $self->_the_RDB->connect();

       my $stat = "select pfamseq_id, seq_start, seq_end from $table where pfamA_acc = ?";

       my $handle = $dbh->prepare($stat);
       $handle->execute($self->acc());

       while ( ($seqid, $st, $en)
	       = $handle->fetchrow) {
	   push @list, "$seqid/$st-$en";

       }
       $handle->finish;    
   };
   $@ and $self->throw("Bio::Pfam::EntryA_RDB->annotated_regions - RDB error $@");

   return @list;
}


sub hmmbuild_ls {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'hmmbuild_ls'} = $value;
   }  else {
       $self->_before_annotation_hook('hmmbuild_ls');
   }
   return $self->{'hmmbuild_ls'};

}

sub hmmcalibrate_ls {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'hmmcalibrate_ls'} = $value;
   }  else {
       $self->_before_annotation_hook('hmmcalibrate_ls');
   }
   return $self->{'hmmcalibrate_ls'};

}

sub hmmbuild_fs {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'hmmbuild_fs'} = $value;
   }  else {
       $self->_before_annotation_hook('hmmbuild_fs');
   }
   return $self->{'hmmbuild_fs'};

}


sub hmmcalibrate_fs {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'hmmcalibrate_fs'} = $value;
   }  else {
       $self->_before_annotation_hook('hmmcalibrate_fs');
   }
   return $self->{'hmmcalibrate_fs'};

}



=head2 name_start_end

 Title   : 
 Usage   : @names = $self->name_start_end('FULL')
 Function:
 Example :
 Returns : a list of "name/start-end" for the alignment of choice 
 Args    :


=cut

sub _the_RDB {
   my ($self,$rdb) = @_;

   if( $rdb) {
       $self->{'_the_rdb'} = $rdb;
   }
   
   return $self->{'_the_rdb'};

}

sub description {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{''} = $value;
   } 
  
   return $self->{'description'};
}
