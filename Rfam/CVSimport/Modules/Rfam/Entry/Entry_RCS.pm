
 


# Let the code begin...


package Rfam::Entry::Entry_RCS;
use vars qw($AUTOLOAD @ISA);

use strict;

use Rfam::Entry::Entry;
use Rfam::RfamRegion;
use Bio::Annotation::Collection;
use Bio::Annotation::DBLink;

@ISA = qw(Rfam::Entry::Entry);



sub new {
  my $caller = shift;
  my $class  = ref( $caller ) || $caller;
  my %arguments = @_;
  
  my $self = $class->SUPER::new(%arguments);

  $self->{'build'}     = [];
  $self->{'forward'}   = [];
  $self->{'reference'} = [];
  $self->{'dblink'}    = [];

  $self->_loaded(0);

  return $self;
}




=head2 add_dblink

 Title   : add_dblink
 Usage   : $self->add_dblink($link)
 Function: add Bio::Annotation::DBLink to the list
 Example :
 Returns : 
 Args    :


=cut

sub add_dblink {
    my ($self,$link) = @_;
    push(@{$self->{'dblink'}},$link);
}


=head2 each_dblink

 Title   : each_dblink
 Usage   : gives you an array of Bio::Annotation::DBLink objects
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub each_dblink{
   my ($self,@args) = @_;
   $self->_before_annotation_hook('dblink');
   return @{$self->{'dblink'}};
}


=head2 add_reference

 Title   : add_reference
 Usage   : $self->add_reference($ref)
 Function: add Bio::Annotation::Reference to the list
 Example :
 Returns : 
 Args    :


=cut

sub add_reference {
    my ($self,$ref) = @_;
    push(@{$self->{'reference'}},$ref);
}


=head2 each_reference

 Title   : each_reference
 Usage   : gives you an array of Bio::Annotation::Reference objects
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub each_reference {
   my ($self,@args) = @_;
   $self->_before_annotation_hook('reference');
   return @{$self->{'reference'}};
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
   } else {
       $self->_before_annotation_hook('en_type');
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
    } else {
	$self->_before_annotation_hook('acc');
    }
  
    return $self->{'acc'};
    
}


sub auto_rfam{
    my ($self,$value) = @_;
    if( defined $value) {
	$self->{'auto_rfam'} = $value;
    } else {
	$self->_before_annotation_hook('auto_rfam');
    }
  
    return $self->{'auto_rfam'};
    
}


sub description{
    my ($self,$value) = @_;
    
    if( defined $value) {
	$self->{'description'} = $value;
    } else {
	$self->_before_annotation_hook('description');
    }
    
    return $self->{'description'};
    
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
   }  else {
       $self->_before_annotation_hook('alignmethod');
   }
    
   return $self->{'alignmethod'};

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
    warn "Rfam::Entry_RCS::ann() is deprecated - you're code is now probably broken!";
    return 0;
#    if( defined $value) {
#	$obj->{'_rcs_ann'} = $value;
#    } else {
#	if( $obj->_loaded() == 0 ) {
#	    $obj->_load_annotation();
#	}
#    }
#    return $obj->{'_rcs_ann'};
}

sub _load_in_full {

  my($self) = @_;
 my($dir, %return);

 $dir = $self->_directory();
 
  $dir = $dir . "/scores";
  open(_SCORES, $dir) || print "Cant find scores file ! \n";
  
  while(<_SCORES>) {
    my($score, $temp_score, $start_end, $mode, $seq);
    if($_ =~ /(\S+)\s+(\S+)\s+(\S+)/) {      
      $temp_score = $1;
      if ($temp_score =~ /^\W+0\.0$/) {
	if ( ($temp_score =~ /^\-/) ||($temp_score =~ /^\+/) ) {
		  
	  $score =  substr($temp_score, 1);
	} else {
	  $score = $temp_score;
	}
    
      } else {
	$score = $temp_score;
      }

      my ($seq, $start_end) = split(/\//, $2);
      $mode = $3;

      if ($score =~ /\.\d\d/) {
	chop($score);
      }

     
      my $all = "$score~$seq~$start_end~$mode";

 
      $return{$all} = $all;

    }

  }
 

  close(_SCORES);
  return %return;
}









=head2 annotated_regions

 Title   : 
 Usage   : @regs = $self->pfam_regions
 Function:
 Example :
 Args    : 'SEED' or 'FULL'
 Returns : A list of AnnotatedRegions for the entry ABOVE the cutoff


=cut

sub annotated_regions {
    my ($self, $type) = @_;
    my ($dir, $acc, $id, $ga, $ann, %output, %nses, @list);
   
    $dir = $self->_directory();
    $acc = $self->acc();
    $id  = $self->id();
    $ga  = $self->gathering_cutoff();

    
   for ($self->name_start_end($type)) {
	$nses{$_} = 1;
    }
   
    #$ann = Bio::Annotation->new('-DESCRIPTION' => $self->ann->description);
    foreach my $region_names (sort keys %nses) {
  
      my ($embl_acc, $seq_start, $seq_end, $bits);

      
      if ($region_names  =~ /^(\S+)\/(\d+)\-(\d+)/) {

	$embl_acc = $1; $seq_start = $2; $seq_end = $3;

     } elsif ($region_names =~ /^(\d+\.\d+)\s+(\S+)\/(\d+)\-(\d+)/) {
       $bits = $1; $embl_acc = $2; $seq_start = $3; $seq_end = $4;
      }

      
      eval {
	    push @list,  Rfam::RfamRegion->new('-RFAM_ACCESSION' => $acc,
					       '-RFAM_ID' => $id,
					       '-SEQ_ID' => $embl_acc,
					       '-FROM' => $seq_start, 
					       '-TO' => $seq_end,
					       '-BITS' => $bits,
					       '-ANNOTATION' => $ann);

	  };
      if ($@) {
	print "BARFED AS $@ \n"; exit(0);
      }
    
    }
    return @list;
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
   }  else {
       $self->_before_annotation_hook('author');
   }
   
   return $self->{'author'};

}



=head2 full

 Title   : full
 Usage   : $full = $self->full()
 Function:
 Example :
 Returns : Rfam::RfamAlign object of the full alignment
 Args    :


=cut

sub full{
   my ($self,@args) = @_;
   my($dir,$id,$out);
   $dir = $self->_directory();
   $id  = $self->id();

   open(_ALIGN,"$dir/ALIGN") || die("For entry object [$id], got no valid directory for ALIGN alignment [$dir/FULL] $!");
   
   $out = Rfam::RfamAlign->new();
   $out->read_stockholm(\*_ALIGN);

   close(_ALIGN);

   return $out;
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
   }  else {
       $self->_before_annotation_hook('gathering');
   }
  
   return $self->{'gathering'};

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
   }  else {
       $self->_before_annotation_hook('id');
   }
   
   return $self->{'id'};
   
}


=head2 internal_comment

 Title   : internal_comment
 Usage   : $self->internal_comment($com)
 Function: 
 Example : 
 Returns : Ref. to Bio::Annotation::Comment
 Args    : bew Comment (optional)


=cut

sub internal_comment {
   my ($self,$value) = @_;

   if (defined $value) {
       $self->{'internal_comment'} = $value;
   }
   return $self->{'internal_comment'};
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
   else {
       $self->_before_annotation_hook('is_dead');
   }
   
   return $self->{'is_dead'};

}



=head2 model_length

 Title   : model_length
 Usage   : $self->model_length($len)
 Function: gets/sets the length of the HMM model (match states) for this pfam familiy
 Returns : 
 Args    :


=cut

sub model_length {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'model_len'} = $value;
   }  else {
       $self->_before_annotation_hook('model_len');
   }
    
    return $self->{'model_len'};

}



=head2 name_start_end

 Title   : 
 Usage   : @names = $self->name_start_end('FULL')
 Function:
 Example :
 Returns : list of name/start-end scalars
 Args    :


=cut

sub name_start_end {
   my ($self,$type) = @_;

   my($dir,$id,$out,$file,@list,$name);

   $dir = $self->_directory();

   $id  = $self->id();
 #  $type = "FULL";
   SWITCH : { 
     $type =~ /FULL/i && do { $file = 'scores'; last; };
     $type =~ /SEED/i && do { $file = 'SEED'; last; };
     die("This type [$type] is neither full nor seed. Can't process");
   }
 
   open(_ALIGN,"$dir/$file") || die("For entry object [$id], got no valid directory for alignment [$dir/$file] $!");
   my (%distinct);
   while(<_ALIGN>) {
     if ($_ =~ /^(\S+\/\d+\-\d+)\s+/) {
       $distinct{$1} = $1;
     } elsif ($_ =~ /^(\d+\.\d+\s+\S+\/\d+\-\d+)/) {
       $distinct{$1} = $1;
     }

     
   }

   close(_ALIGN) || die("Could not close alignment file handle");

   @list = keys %distinct;

   return @list;
}



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
   }  else {
       $self->_before_annotation_hook('noise_cutoff');
   }
   
    return $self->{'noise_cutoff'};

}







=head2 num_seqs_in_full

 Title   : 
 Usage   : $count = $self->num_seqs_in_full
 Function:
 Example :
 Returns : The number of sequences in the full alignment for the entry
 Args    : The number of sequences in the full alignment for the entry (optional)


=cut

sub num_seqs_in_full {
   my ($self, $value) = @_;
   if (defined $value) {
       $self->{'num_seqs_in_full'} = $value;
   }
   return $self->{'num_seqs_in_full'}
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
   if (defined $value) {
       $self->{'num_seqs_in_seed'} = $value;
   }
   return $self->{'num_seqs_in_seed'};
}


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
    }  else {
	$obj->_before_annotation_hook('previous_ids');
    }
    return $obj->{'previous_ids'};

}



=head2 seed

 Title   : seed
 Usage   : $seed = $self->seed()
 Function:
 Example :
 Returns : Rfam::RfamAlign object of the seed alignment
 Args    :


=cut

sub seed{
   my ($self,@args) = @_;

   my($dir,$id,$out);
   $dir = $self->_directory();
   $id  = $self->id();

   open(_SEED,"$dir/SEED") || die("For entry object [$id], got no valid directory for SEED alignment [$dir/SEED] $!");
   
   $out = Rfam::RfamAlign->new();
   $out->read_stockholm(\*_SEED);

   close(_SEED);

   return $out;
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
   }  else {
       $self->_before_annotation_hook('source');
   }

   return $self->{'source'};

}


sub comment{
   my ($self,$value) = @_;
   if( defined $value) {
       $self->{'comment'} = $value;
   }  else {
       $self->_before_annotation_hook('comment');
   }

   return $self->{'comment'};

}



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
   }  else {
       $self->_before_annotation_hook('trusted_cutoff');
   }
    
    return $self->{'trusted_cutoff'};

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

   if( $type eq 'id' ) {
       return 1;
   }

   if( $self->_loaded() == 1 ) {
       return 1;
   }

   return $self->_load_annotation();
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
  
   my($dir,$id,$str,$fname,@hname);
 
   $dir = $self->_directory();
   $fname = $self->_desc_filename;
  
   $id = $self->id();

   if( $self->_loaded() == 1) {
       $self->warn("Reloading annotation without indicating deloaded object.");
   }
 
   open(_SOURCE,"$dir/$fname") || print "For entry object [$id], got no valid directory for $fname [$dir/$fname] $!";

   $self->_read_std_desc(\*_SOURCE);
   close(_SOURCE) || die("Could not close [$id] DESC in reading annotation");
   
   $self->_loaded(1);

   # if we have a dead family then we don't want to go further
   if( $self->is_dead ) {
       return 1;
   }

   my($num_seed, $num_full, %distinct_seed);
   $num_full = $num_seed = 0;
   open(_SEED, "$dir/SEED");
   while(<_SEED>) {
     $distinct_seed{$1} = $1 if ($_ =~ /^(\S+\/\d+\-\d+)/);
     
   }
   close(_SEED);
   foreach (sort keys %distinct_seed) {
     $num_seed++;
   }
   $self->num_seqs_in_seed($num_seed);

   open(_FULL, "$dir/scores");
   while(<_FULL>) {
     $num_full++;
     
   }
   close(_FULL);
   $self->num_seqs_in_full($num_full);

   return 1;
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
   if( defined $value) {
      $obj->{'_loaded'} = $value;
    }
    return $obj->{'_loaded'};

}



=head2 _directory

 Title   : _directory
 Usage   : $obj->_directory($newval)
 Function: Internal directory for the entry
 Example : 
 Returns : value of _directory
 Args    : newvalue (optional)


=cut

sub _directory{
    my ($self,$value) = @_;
    if( defined $value) {
	$self->{'_directory'} = $value;
    }

    return $self->{'_directory'};

}



=head2 _desc_filename

 Title   : _desc_filename
 Usage   : $obj->_directory($newval)
 Function: Filename of where annotation is stored
 Example : 
 Returns : value of _desc_filename
 Args    : newvalue (optional)


=cut

sub _desc_filename {
    my ($self,$value) = @_;
    if( defined $value) {
	$self->{'_desc_filename'} = $value;
    }
    return $self->{'_desc_filename'};
    
}

