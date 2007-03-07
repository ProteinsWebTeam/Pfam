
#
# BioPerl module for Bio::Pfam::EntryA_Web
#
# Cared for by Ewan Birney <pfam@sanger.ac.uk>
#
# Copyright Ewan Birney
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::EntryA_Web - DESCRIPTION

Concrete implementation of a Pfam-A entry. This particular implementation
is for those entries that 'exist' in a Pfam web-site

=head1 SYNOPSIS

The user shoud not instantiate this object directly, but will tranparently
receive t via a call to DB::get_EntryA_by_acc (or id)

=head1 DESCRIPTION

On a Pfam web-site, that data is split across directories, but rather than
one directory for each family, we have one directory for each type of data.
(One for descriptions, one for seed alignments, one for full alignments, etc).
Each directory contains a separate file for each family

=head1 CONTACT

pfam@sanger.ac.uk

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...

# Author: rdf

package Bio::Pfam::EntryA_Web;
use vars qw($AUTOLOAD @ISA);
use strict;
use warnings;

use Bio::Pfam::AlignPfam;
use Bio::Pfam::EntryA;

@ISA = qw(Bio::Pfam::EntryA);

sub new {
  my($class,@args) = @_;
  my $self = $class->SUPER::new(@args);

  $self->{'build'} = [];
  $self->{'forward'} = [];
  $self->_loaded(0);

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
   push(@{$self->{'build'}},$line);

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
   $self->_before_annotation_hook('build');
   return @{$self->{'build'}};
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

      } elsif( !$self->from_rdb()  ) {
       $self->_before_annotation_hook('entry_type');
   }
  
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

   push(@{$self->{'forward'}},$acc);

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
   $self->_before_annotation_hook('forward');
   return @{$self->{'forward'}};
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
      
      }elsif( !$self->from_rdb()  ) {
	
       $self->_before_annotation_hook('acc');
   }
    
    return $self->{'acc'};
    
}


=head2 annotated_regions

 Title   : 
 Usage   : @regs = $self->pfam_regions
 Function:
 Example :
 Returns : A list of AnnotatedRegion for the entry
 Notes   : 
    The regions returned will have undefined fields for the
    hmm start-ends. This information is not available on
    the web-site

=cut

sub annotated_regions {
   my ($self, $type) = @_;
   my ($acc, $id, $ann, @list);

   $acc = $self->acc();
   $id  = $self->id();
   
   $ann = $self->description;
   
   foreach my $nse ($self->name_start_end($type)) {
       if ($nse =~ /^(\S+)\/(\d+)\-(\d+)/) {
	   push @list,  Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $acc,
						   '-PFAM_ID' => $id,
						   '-SEQ_ID' => $1,
						   '-FROM' => $2, 
						   '-TO' => $3,
						   '-ANNOTATION' => $ann
						   );
       }
   }   
   
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
   }  elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('id');
   }
   return $self->{'id'};
   
}


sub from_rdb{
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'from_rdb'} = $value;
   } 
  
   return $self->{'from_rdb'};
   
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
   }  elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('author');
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
   }elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('model_length');
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
   }  elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('source');
   }
    
   return $self->{'source'};

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
   } elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('alignmethod');
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


   if( defined $value) {
       $self->{'gathering'} = $value;

       if ($value =~ /;/) {
	 $value =~ s/;//g; 
	 my($ls_sequence_GA, $ls_domain_GA,$fs_sequence_GA, $fs_domain_GA) = split (/\s+/, $value);
	 $self->ls_sequence_gathering_cutoff($ls_sequence_GA);
	 $self->ls_domain_gathering_cutoff($ls_domain_GA);
	 $self->fs_sequence_gathering_cutoff($fs_sequence_GA);
	 $self->fs_domain_gathering_cutoff($fs_domain_GA);
       }
  } elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('gathering');
   }
    
    return $self->{'gathering'};

}


sub ls_sequence_gathering_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'ls_sequence_gathering'} = $value;
   } elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('ls_sequence_gathering');
   }
   return $self->{'ls_sequence_gathering'};
}


sub ls_domain_gathering_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'ls_domain_gathering'} = $value;
  }  elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('ls_domain_gathering');
   }
   return $self->{'ls_domain_gathering'};
}

sub fs_sequence_gathering_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'fs_sequence_gathering'} = $value;
   }  elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('fs_sequence_gathering');
   }
   return $self->{'fs_sequence_gathering'};

}


sub fs_domain_gathering_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'fs_domain_gathering'} = $value;
   }  elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('fs_domain_gathering');
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

   if( defined $value) {
       $self->{'trusted_cutoff'} = $value;
       if ($value =~ /;/) {
	 $value =~ s/;//g; 
	 my($ls_sequence_TC, $ls_domain_TC,$fs_sequence_TC, $fs_domain_TC) = split (/\s+/, $value);
	 $self->ls_sequence_trusted_cutoff($ls_sequence_TC);
	 $self->ls_domain_trusted_cutoff($ls_domain_TC);
	 $self->fs_sequence_trusted_cutoff($fs_sequence_TC);
	 $self->fs_domain_trusted_cutoff($fs_domain_TC);
       }
         }  elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('trusted_cutoff');
   }
    
    return $self->{'trusted_cutoff'};

}



##################
# NEW TRUSTED PARAMETERS
######

sub ls_sequence_trusted_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'ls_sequence_trusted'} = $value;
       
  }  elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('ls_sequence_trusted');
   }
   return $self->{'ls_sequence_trusted'};
}


sub ls_domain_trusted_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'ls_domain_trusted'} = $value;
   }  elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('ls_domain_trusted');
   }
   return $self->{'ls_domain_trusted'};
}

sub fs_sequence_trusted_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'fs_sequence_trusted'} = $value;
   } elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('fs_sequence_trusted');
   }
   return $self->{'fs_sequence_trusted'};

}


sub fs_domain_trusted_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'fs_domain_trusted'} = $value;
   } elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('');
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

   if( defined $value) {
     $self->{'noise_cutoff'} = $value;
     if ($value =~ /;/) {
       $value =~ s/;//g; 
       my($ls_sequence_NC, $ls_domain_NC,$fs_sequence_NC, $fs_domain_NC) = split (/\s+/, $value);
       $self->ls_sequence_noise_cutoff($ls_sequence_NC);
       $self->ls_domain_noise_cutoff($ls_domain_NC);
       $self->fs_sequence_noise_cutoff($fs_sequence_NC);
       $self->fs_domain_noise_cutoff($fs_domain_NC);
     }
       }  elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('noise_cutoff');
   }
   
    return $self->{'noise_cutoff'};

}





##################
# NEW NOISE PARAMETERS
######

sub ls_sequence_noise_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'ls_sequence_noise'} = $value;
   } elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('id');
   }
   return $self->{'ls_sequence_noise'};
}

sub ls_domain_noise_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'ls_domain_noise'} = $value;
   } elsif( !$self->from_rdb()  ) {

      $self->_before_annotation_hook('id');
   }
   return $self->{'ls_domain_noise'};
}

sub fs_sequence_noise_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'fs_sequence_noise'} = $value;
   } elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('id');
   }
   return $self->{'fs_sequence_noise'};

}


sub fs_domain_noise_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'fs_domain_noise'} = $value;
   } elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('id');
   }
   return $self->{'fs_domain_noise'};

}

#/ end new NOISE PARAMS



### BUILD SUBS

sub hmmbuild_ls {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'hmmbuild_ls'} = $value;
   } elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('id');
   }
   return $self->{'hmmbuild_ls'};

}


sub hmmcalibrate_ls {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'hmmcalibrate_ls'} = $value;
   } elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('id');
   }
   return $self->{'hmmcalibrate_ls'};

}

sub  hmmbuild_fs{
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'hmmbuild_fs'} = $value;
   } elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('id');
   }
   return $self->{'hmmbuild_fs'};

}

sub hmmcalibrate_fs {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'hmmcalibrate_fs'} = $value;
   } elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('id');
   }
   return $self->{'hmmcalibrate_fs'};

}


#### /end build subs


=head2 previous_ids

 Title   : previous_ids
 Usage   : $obj->previous_ids($newval)
 Function: 
 Example : 
 Returns : value of previous_ids
 Args    : newvalue (optional)


=cut

sub previous_ids{
    my ($obj,$value) = @_;
    
    if( defined $value) {
	$obj->{'previous_ids'} = $value;
   }  elsif( !$obj->from_rdb()  ) {

       $obj->_before_annotation_hook('previous_ids');
   }
    return $obj->{'previous_ids'};

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
   }elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('is_dead');
   }

   
   return $self->{'is_dead'};

}
  

sub arch_in_seed  {
   my ($self, $value) = @_;

   if( defined $value) {
       $self->{'arch_in_seed'} = $value;
   }elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('arch_in_seed');
   } 
   
   return $self->{'arch_in_seed'};
}

sub arch_in_full  {
   my ($self, $value) = @_;

   if( defined $value) {
       $self->{'arch_in_full'} = $value;
   }elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('arch_in_full');
   } 
   
   return $self->{'arch_in_full'};
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
   }elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('num_seqs_in_full');
   } 
   
   return $self->{'num_seqs_in_full'};
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
   }elsif( !$self->from_rdb()  ) {

       $self->_before_annotation_hook('num_seqs_in_seed');
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
   my($dir,$id,$out,$acc);
   $dir = $self->_datadir();
   $acc  = $self->acc();
   #print "Content-type: text/html\n\n"; print "WOWSER <P>";
   open(_ALIGN,"gunzip -c $dir/seed/$acc.full.gz |") || $self->throw("For entry object [$id], got no valid directory for SEED alignment [$dir/seed/$acc.full.gz] $!");
   $out = Bio::Pfam::AlignPfam->new();
   $out->read_stockholm(\*_ALIGN);

   close(_ALIGN);
   return $out;
					    
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
   my($dir,$id,$out,$acc);

   $dir = $self->_datadir();
   $acc  = $self->acc();
  # print "Content-type: text/html\n\n"; print "WOWSER <P>";
   open(_ALIGN,"gunzip -c $dir/full/$acc.full.gz |") || $self->throw("For entry object [$id], got no valid directory for ALIGN alignment [$dir/SEED] $!");
   
   $out = Bio::Pfam::AlignPfam->new();
   $out->read_stockholm(\*_ALIGN);

   close(_ALIGN);

   return $out;
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
    } else {
	if($obj->_loaded() == 0) {
	    $obj->_load_annotation();
	}
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

   my($dir,$id,$out,$file,$name, $acc, @list);

   $dir = $self->_datadir();
   $acc  = $self->acc();


   SWITCH : { 
     $type =~ /FULL/ && do { $file = 'full'; last; };
     $type =~ /SEED/ && do { $file = 'seed'; last; };
     $self->throw("This type [$type] is neither full nor seed. Can't process");
   }
 

   open(_ALIGN,"gunzip -c $dir/$file/$acc.full.gz |") || $self->throw("For entry object [$id], got no valid directory for ALIGN alignment [$dir/SEED] $!");
   
   while(<_ALIGN>) {
     ($name) = split;
     push(@list,$name);
   }

   close(_ALIGN) || $self->throw("Could not close alignment file handle (pipe for gunzip)");

   return @list;
}


=head2 _datadir

 Title   : _datadir
 Usage   : $obj->_directory($newval)
 Function: Internal data directory for the entry
 Example : 
 Returns : value of _datadir
 Args    : newvalue (optional)


=cut

sub _datadir{
   my ($self,$value) = @_;
   if( defined $value) {
      $self->{'_datadir'} = $value;
    }
    return $self->{'_datadir'};

}

=head2 _before_annotation_hook

 Title   : _before_annotation_hook
 Usage   :
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub _before_annotation_hook{
   my ($self,$type) = @_;

   if( $type eq 'acc' ) {
       return 1;
   }

   if( $self->_loaded() == 1) {
       return 1;
   }

   return $self->_load_annotation();
}


=head2 _loaded

 Title   : _loaded
 Usage   : $obj->_loaded($newval)
 Function: Indicates whether the annotation has been loaded or not
 Example : 
 Returns : value of _loaded
 Args    : newvalue (optional)


=cut

sub _loaded{
   my ($obj,$value) = @_;
   if( $value) {
      $obj->{'_loaded'} = $value;
    }
    return $obj->{'_loaded'};

}


=head2 _load_annotation

 Title   : _load_annotation
 Usage   : $self->_load_annotation()
 Function: 
 Example : 
 Returns : reads in annotation
 Args    : 


=cut

sub _load_annotation {
   my ($self,$value) = @_;
   my($dir,$acc);
 

   if( $self->_loaded() == 1) {
       $self->warn("Reloading annotation without indicating deloaded object.");
   }

   $dir = $self->_datadir();
   $acc  = $self->acc();

   open(_DESC,"$dir/desc/$acc.desc") || $self->throw("For entry object [$acc], got no valid directory for desc file [$dir/desc/$acc.desc] $!");
 
  
   $self->ann(Bio::Annotation::Collection->new());
   $self->_read_std_desc(\*_DESC,$self->{'_web_ann'});

   close(_DESC) || $self->throw("Could not close DESC in readin annotation");
   $self->_loaded(1);
 
   return 1;
     
}



######### NEW SUBS ######

sub _load_refs_from_rdb {
  my ($self,$comment, $description,  @refs) = @_;
   
 $self->ann(Bio::Annotation::Collection->new());
  
  $self->_read_rdb_desc( $self->{'_web_ann'},$comment, $description,  @refs);
$self->_loaded(1);
}




sub auto_pfamA{
    my ($self,$value) = @_;
    
    if( defined $value) {
	$self->{'auto_pfamA'} = $value;
    } 
    
    return $self->{'auto_pfamA'};
    
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

sub comment{
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'comment'} = $value;
   } 
    
   return $self->{'comment'};

}


sub description {
   my ($self,$value) = @_;
   if( defined $value) {
       $self->{'description'} = $value;
   } 
  
   return $self->{'description'};
}


sub average_length {
   my ($self,$value) = @_;
   if( defined $value) {
       $self->{'average_length'} = $value;
   } 
  
   return $self->{'average_length'};
}

sub percentage_id {
   my ($self,$value) = @_;
   if( defined $value) {
       $self->{'percentage_id'} = $value;
   } 
  
   return $self->{'percentage_id'};
}

sub  average_coverage {
   my ($self,$value) = @_;
   if( defined $value) {
       $self->{'average_coverage'} = $value;
   } 
  
   return $self->{'average_coverage'};
}
