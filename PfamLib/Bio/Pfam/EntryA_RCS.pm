
#
# BioPerl module for Bio::Pfam::EntryA_Dir
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::EntryA_RCS - Concrete implementation of a Pfam-A entry


=head1 DESCRIPTION

This class is a concrete Pfam-A entry, as constructed and returned by 
DB_RCS.pm. It encapsulates Entries that are implemented by the 'rcs-files
in directory layout' database system

=head1 CONTACT

pfam@sanger.ac.uk

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut
 


# Let the code begin...


package Bio::Pfam::EntryA_RCS;
use vars qw($AUTOLOAD @ISA);

use strict;

use Bio::Pfam::AlignPfam;
use Bio::Pfam::EntryA;
use Bio::Pfam::PfamRegion;
use HMMResults;

@ISA = qw(Bio::Pfam::EntryA);

sub new {
  my($class,@args) = @_;
  my $self = $class->SUPER::new(@args);

  $self->{'build'}   = [];
  $self->{'forward'} = [];
  $self->{'edit'}    = [];

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
 
   if ($line =~ /HMM\_ls/i) {
   
     if ($line =~ /hmmbuild/i) {
       $self->hmmbuild_ls($line);
     } elsif ($line =~ /hmmcalibrate/i) {
       $self->hmmcalibrate_ls($line);
     }

     
   } elsif ($line =~ /HMM\_fs/i) {
 
     if ($line =~ /hmmbuild/i) {
       $self->hmmbuild_fs($line);
     } elsif ($line =~ /hmmcalibrate/i) {
       $self->hmmcalibrate_fs($line);
     }

   }

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
   if( $self->{'build'}){
	return @{$self->{'build'}};
	}
}


=head2 modify_build_line

 Title   : modify_build_line
 Usage   :  
 Function: removes '--cpu 1' from build line
 Example :
 Returns : 
 Args    :


=cut

sub modify_build_line{
   my $self = shift;
   # before we access the build lines directly, we need to
   # load them (cunning _before_annotation_hook confusion)
   # so run each_build_line
   $self->each_build_line;
   my @tmp_array = @{$self->{'build'}};
   $self->{'build'}   = [];

   foreach my $line (@tmp_array) {
       if($line =~ /\-\-cpu 1 /) {
         $line =~ s/\-\-cpu 1 //;

       }
       push(@{$self->{'build'}},$line);
   }
}



=head2 add_edit_line

 Title   : add_edit_line
 Usage   : $self->add_edit_line($line)
 Function: adds another edit line to the object
 Example :
 Returns : 
 Args    :


=cut

sub add_edit_line {
   my ($self,$line) = @_;
   push(@{$self->{'edit'}},$line);
}


=head2 each_edit_line

 Title   : each_edit_line
 Usage   : gives you an array of edit lines
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub each_edit_line{
   my ($self,@args) = @_;
   $self->_before_annotation_hook('edit');
   if ($self->{'edit'}){
	return @{$self->{'edit'}};
	}
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

sub auto_pfamA{
    my ($self,$value) = @_;
    
    if( defined $value) {
	$self->{'auto_pfamA'} = $value;
    } else {
	$self->_before_annotation_hook('auto_pfamA');
    }
    
    return $self->{'auto_pfamA'};
    
}


=head2 nested_domains
 Usage   : $count = $self->nested_domains(@_);
 Function:
 Example : 
 Returns : The Pfam ids that this entry isallowed to overlap with
 Args    :
=cut


sub nested_domains {
	
  my ($self, $domain, $location) = @_;

  if ($domain && $location){
	  push(@{$self->{'nested_domains'}},{ 'domain' => $domain, 'location'=> $location}); 
    
  } else {
    $self->_before_annotation_hook('nested_domains');
  }


  if (defined $self->{'nested_domains'}) {
     return @{$self->{'nested_domains'}};
   
  } else {
     return ();
  }

  
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


=head2 alignorder

 Title   : alignorder
 Usage   : $self->alignorder($newval)
 Function: 
 Example : 
 Returns : value of alignorder
 Args    : newvalue (optional)

=cut

sub alignorder{
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'alignorder'} = $value;
   }  else {
       $self->_before_annotation_hook('alignorder');
   }
    
   return $self->{'alignorder'};

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
    if( defined $value) {
	$obj->{'_rcs_ann'} = $value;
    } else {
	if(!$obj->_loaded()) {
	    $obj->_load_annotation();
		}
    }

    return $obj->{'_rcs_ann'};

}

sub _load_in_full {

    my($self) = @_;
    my($dir, %return);
    
    $dir = $self->_directory();
    
    $dir = $dir . "/scores";
    open(_SCORES, $dir) || print "Cant find scores file ! [$!] [$dir] \n";
    
    while(<_SCORES>) {
	my($score, $temp_score, $start_end, $mode, $seq);
	if($_ =~ /(\S+)\s+(\S+)\s+(\S+)/) {      
	    $temp_score = $1;
	    
	    my $seq_start_end = $2;
	    $mode = $3;
	    if ($temp_score =~ /\W0\.0/) {
		if ( ($temp_score =~ /^\-/) ||($temp_score =~ /^\+/) ) {
		    
		    $score =  substr($temp_score, 1);
		} else {
		    $score = $temp_score;
		}
		
	    } else {
		$score = $temp_score;
	    }
	    
	    my ($seq, $start_end) = split(/\//, $seq_start_end);
    
	    
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




=head2 all_full_annotated_regions

 Title   : 
 Usage   : @regs = $self->pfam_regions
 Function:
 Example :
 Args    : 
 Returns : A list of ALL AnnotatedRegions - ls, fs and below cutoff for the entry 


=cut

sub all_full_annotated_regions {
    my $self = shift;
    my ($dir, $acc, $id, $ga, $ann, %output, %nses, @list, %in_full_align_values);
    
    $dir = $self->_directory();
    $acc = $self->acc();
    $id  = $self->id();
    $ga  = $self->gathering_cutoff();
    $_ = $ga;
    my( $seqthrls,$domthrls,$seqthrfs,$domthrfs ) = /^(\S+)\s+(\S+);\s+(\S+)\s+(\S+);\s*$/;
    
    my %edit;
    foreach ( $self->each_edit_line() ) {     # read in edit lines
	print STDERR "reading edit [$_]\n";
	if( /^(\S+\/\d+-\d+)\;\s*\S+\/(\d+)-(\d+)\;/ ) {
	    $edit{ $1 } = { 'start' => $2,
			    'end'   => $3 };
	}
	elsif( /^(\S+\/\d+-\d+)\;/ ) {
	    $edit{ $1 } = { 'start' => undef,
			    'end'   => undef }
	}
    }

    %in_full_align_values = $self->_load_in_full();
    
	$ann = $self->description;
    
    foreach my $mode ( "ls", "fs" ) {
	my( $seqthr, $domthr );
	if( $mode eq "ls" ) {
	    $seqthr = $seqthrls;
	    $domthr = $domthrls;
	}
	elsif( $mode eq "fs" ) {
	    $seqthr = $seqthrfs;
	    $domthr = $domthrfs;
	}

	open(_OUTPUT, "$dir/PFAMOUT_$mode") || $self->throw("Could not open file $dir/PFAMOUT_$mode");
	my $res = new HMMResults;
	$res->parse_pfam(\*_OUTPUT);   
    
	foreach my $seq ( $res->eachHMMSequence() ) {
	    $seq->evalue("0.0") if !$seq->evalue;
	    $seq->bits("0.0")   if !$seq->bits;
	
	    foreach my $unit ( $seq->eachHMMUnit() ) {
		my $tempnse = $unit->seqname."/".$unit->start_seq."-".$unit->end_seq;
		if( exists $edit{ $tempnse } ) {
		    next if( not defined $edit{$tempnse}->{'start'} );  # skip deleted unit
		    $unit->start_seq( $edit{$tempnse}->{'start'} );     # fudge changed units
		    $unit->end_seq(   $edit{$tempnse}->{'end'}   );
		}

		$unit->evalue("0.0") if !$unit->evalue;
		$unit->bits("0.0")   if !$unit->bits;
	    
		my $score;
		if ($unit->bits =~ /\W0\.0/) {
		    if ( ($unit->bits =~ /^\-/) ||($unit->bits =~ /^\+/) ) {
			$score =  substr($unit->bits, 1);
		    } else {
			$score = $unit->bits;
		    }
		    
		} else {
		    $score = $unit->bits;
		}
	    
		my $score_test = $score . "~" . $unit->seqname ."~" . $unit->start_seq . "-" . $unit->end_seq . "~" . $mode;
	    
		my $in_full = 0;
		if (defined($in_full_align_values{$score_test})) {
		    $in_full_align_values{$score_test} = 1;
		    $in_full = 1;
		}
	    
		my ($is_significant) = 0;
		$is_significant = 1 if ( ($unit->bits >= $domthr) && ($seq->bits  >= $seqthr));
	    
		$nses{$unit->seqname."/".$unit->start_seq."-".$unit->end_seq} = $unit->seqname."/".$unit->start_seq."-".$unit->end_seq;
		$output{$unit->seqname."/".$unit->start_seq."-".$unit->end_seq. "~" . $mode} = {'model_st' => $unit->start_hmm,
												'model_en' => $unit->end_hmm,
												'bits' => $unit->bits,
												'evalue' => $unit->evalue,
												'mode' => $mode,	  
												'seq_bits' => $seq->bits,
												'seq_evalue' => $seq->evalue,
												'is_significant' => $is_significant,
												'in_full' => $in_full	 
												};
	    }
	}
	close(_OUTPUT) || $self->throw("Could not close file [$dir/PFAMOUT_$mode]");
    }
    
    foreach my $key (keys %in_full_align_values) {
	if ($in_full_align_values{$key} != 1) {
	    print "ERROR: full alignment $key hasnt been loaded up to RDB. This normally occurs when the domains in the scores file dont match the ones in PFAMOUT_ls/PFAMOUT_fs. [script EntryA_RCS]. Talk to someone if you are unsure what to do!!!\n";
	}
    }

    foreach my $el (keys %nses) {
	
	my @mode = ('~ls', '~fs');
	
	my ($name, $seq_st, $seq_en) = ($el =~ /^(\S+)\/(\d+)\-(\d+)$/);
	foreach (@mode) {
	    my ($el_with_mode) = $el . $_;
	    my ($hmm_st, $hmm_en, $bits, $evalue,$seq_bits, $seq_evalue, $mode, $is_significant, $in_full);
	    
	    if (defined $output{$el_with_mode}) {
		#Okay, lets copy the hash reference to another hash
		my %tmp = %{$output{$el_with_mode}};
		
		#Lets use up some more memory and time
		$hmm_st = $tmp{'model_st'};
		$hmm_en = $tmp{'model_en'};
		$bits = $tmp{'bits'};
		$evalue = $tmp{'evalue'};
		$seq_bits = $tmp{'seq_bits'};
		$seq_evalue = $tmp{'seq_evalue'};
		$mode = $tmp{'mode'};
		$is_significant = $tmp{'is_significant'};
		$in_full = $tmp{'in_full'};
		
		#now start using this stuff
		push @list,  Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $acc,
							'-PFAM_ID' => $id,
							'-SEQ_ID' => $name,
							'-FROM' => $seq_st, 
							'-TO' => $seq_en,
							
							'-BITS' => $bits,
							'-EVALUE' => $evalue,
							'-DOMAIN_BITS' => $bits,
							'-DOMAIN_EVALUE' => $evalue,
							
							'-MODEL_FROM' => $hmm_st, 
							'-MODEL_TO' => $hmm_en,
							'-ANNOTATION' => $ann,
							'-MODE' => $mode,
							'-SEQUENCE_BITS' =>  $seq_bits,
							'-SEQUENCE_EVALUE' =>  $seq_evalue,
							'-IS_SIGNIFICANT' => $is_significant ,
							'-IN_FULL' => $in_full
							);
		
	    }
	    
	}
	
    }
    
    return @list;
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
    $_ = $ga;
    my($seqthrls,$domthrls,$seqthrfs,$domthrfs ) = /^(\S+)\s+(\S+);\s+(\S+)\s+(\S+);\s*$/;
    
    for ($self->name_start_end($type)) {
	$nses{$_} = 1;
    }
  	$ann = $self->description;
    #$ann = Bio::Annotation->new('-DESCRIPTION' => $self->ann->description);
    if ($type =~ /FULL/i) {
	foreach my $mode ( "ls", "fs" ) {
	    my( $seqthr, $domthr );
	    if( $mode eq "ls" ) {
		$seqthr = $seqthrls;
		$domthr = $domthrls;
	    }
	    elsif( $mode eq "fs" ) {
		$seqthr = $seqthrfs;
		$domthr = $domthrfs;
	    }

	    open(_OUTPUT, "$dir/PFAMOUT_$mode") || $self->throw("Could not open file $dir/PFAMOUT_$mode");
	    my $res = new HMMResults;
	    $res->parse_pfam(\*_OUTPUT);
	    my $new = $res->filter_on_cutoff($seqthr,$domthr);
	    foreach my $seq ( $new->eachHMMSequence() ) {
		foreach my $unit ( $seq->eachHMMUnit() ) {
		    $unit->evalue("0.0") if !$unit->evalue;
		    $unit->bits("0.0")   if !$unit->bits;
		    $output{$unit->seqname."/".$unit->start_seq."-".$unit->end_seq} = {'model_st' => $unit->start_hmm,
										       'model_en' => $unit->end_hmm,
										       'bits' => $unit->bits,
										       'evalue' => $unit->evalue};
		}
	    }
	    close(_OUTPUT) || $self->throw("Could not close file [$dir/PFAMOUT_$mode]");
	}
    }
   
    foreach my $el (keys %nses) {
	my ($name, $seq_st, $seq_en) = ($el =~ /^(\S+)\/(\d+)\-(\d+)$/);
	my ($hmm_st, $hmm_en, $bits, $evalue);
	if ($type =~ /FULL/i) {
	    if (defined $output{$el}) {
		my %tmp = %{$output{$el}};
		$hmm_st = $tmp{'model_st'};
		$hmm_en = $tmp{'model_en'};
		$bits = $tmp{'bits'};
		$evalue = $tmp{'evalue'};
	    }
	    push @list,  Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $acc,
						    '-PFAM_ID' => $id,
						    '-SEQ_ID' => $name,
						    '-FROM' => $seq_st, 
						    '-TO' => $seq_en,
						    '-EVALUE' => $evalue,
						    '-BITS' => $bits,
						    '-MODEL_FROM' => $hmm_st, 
						    '-MODEL_TO' => $hmm_en,
						    '-ANNOTATION' => $ann);
	}
	else {
	    push @list,  Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $acc,
						    '-PFAM_ID' => $id,
						    '-SEQ_ID' => $name,
						    '-FROM' => $seq_st, 
						    '-TO' => $seq_en,
						    '-ANNOTATION' => $ann);
	   
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
 Returns : Bio::Pfam::AlignPfam object of the full alignment
 Args    :


=cut

sub full{
   my ($self,@args) = @_;
   my($dir,$id,$out);
   $dir = $self->_directory();
   $id  = $self->id();

   open(_ALIGN,"$dir/ALIGN") || $self->throw("For entry object [$id], got no valid directory for ALIGN alignment [$dir/FULL] $!");
   
   $out = Bio::Pfam::AlignPfam->new();
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
       $value =~ s/;//g; 
       my($ls_sequence_GA, $ls_domain_GA,$fs_sequence_GA, $fs_domain_GA) = split (/\s+/, $value);
       $self->ls_sequence_gathering_cutoff($ls_sequence_GA);
       $self->ls_domain_gathering_cutoff($ls_domain_GA);
       $self->fs_sequence_gathering_cutoff($fs_sequence_GA);
       $self->fs_domain_gathering_cutoff($fs_domain_GA);
   }  else {
       $self->_before_annotation_hook('gathering');
   }
  
   return $self->{'gathering'};

}


##################
# NEW GATHERING PARAMETERS
######

sub ls_sequence_gathering_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'ls_sequence_gathering'} = $value;
   }  else {
       $self->_before_annotation_hook('ls_sequence_gathering');
   }
   return $self->{'ls_sequence_gathering'};
}


sub ls_domain_gathering_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'ls_domain_gathering'} = $value;
   }  else {
       $self->_before_annotation_hook('ls_domain_gathering');
   }
   return $self->{'ls_domain_gathering'};
}

sub fs_sequence_gathering_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'fs_sequence_gathering'} = $value;
   }  else {
       $self->_before_annotation_hook('fs_sequence_gathering');
   }
   return $self->{'fs_sequence_gathering'};

}


sub fs_domain_gathering_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'fs_domain_gathering'} = $value;
   }  else {
       $self->_before_annotation_hook('fs_domain_gathering');
   }
   return $self->{'fs_domain_gathering'};

}

#/ end new GA PARAMS


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


=head2 add_internal_comment

 Title   : add_internal_comment
 Usage   : $self->add_internal_comment($com)
 Function: 
 Example : 
 Returns : 
 Args    : bew Comment (optional)


=cut

sub add_internal_comment {
   my ($self,$value) = @_;
   if (defined $value) {
       push(@{$self->{'internal_comment'}}, $value);
   }
}

=head2 add_internal_comment

 Title   : each_internal_comment
 Usage   : $self->each_internal_comment($com)
 Function: 
 Example : 
 Returns : Array of comment objects
 Args    : bew Comment (optional)


=cut

sub each_internal_comment {
   my ($self) = @_;
   $self->_before_annotation_hook('internal_comment');
   if (defined $self->{'internal_comment'}){
		return @{$self->{'internal_comment'}};
		}
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

   SWITCH : { 
     $type =~ /FULL/i && do { $file = 'ALIGN'; last; };
     $type =~ /SEED/i && do { $file = 'SEED'; last; };
     $self->throw("This type [$type] is neither full nor seed. Can't process");
   }
 
   open(_ALIGN,"$dir/$file") || $self->throw("For entry object [$id], got no valid directory for alignment [$dir/$file] $!");
   
   while(<_ALIGN>) {
     ($name) = split;
     next if ($name =~ /^\#=RF/);
     push(@list,$name);
   }

   close(_ALIGN) || $self->throw("Could not close alignment file handle");

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
       $value =~ s/;//g; 
       my($ls_sequence_NC, $ls_domain_NC,$fs_sequence_NC, $fs_domain_NC) = split (/\s+/, $value);
       $self->ls_sequence_noise_cutoff($ls_sequence_NC);
       $self->ls_domain_noise_cutoff($ls_domain_NC);
       $self->fs_sequence_noise_cutoff($fs_sequence_NC);
       $self->fs_domain_noise_cutoff($fs_domain_NC);
   }  else {
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
   }  else {
       $self->_before_annotation_hook('ls_sequence_noise');
   }
   return $self->{'ls_sequence_noise'};
}

sub ls_domain_noise_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'ls_domain_noise'} = $value;
   }  else {
       $self->_before_annotation_hook('ls_domain_noise');
   }
   return $self->{'ls_domain_noise'};
}

sub fs_sequence_noise_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'fs_sequence_noise'} = $value;
   }  else {
       $self->_before_annotation_hook('fs_sequence_noise');
   }
   return $self->{'fs_sequence_noise'};

}


sub fs_domain_noise_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'fs_domain_noise'} = $value;
   }  else {
       $self->_before_annotation_hook('fs_domain_noise');
   }
   return $self->{'fs_domain_noise'};

}

#/ end new NOISE PARAMS



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
 Returns : Bio::Pfam::AlignPfam object of the seed alignment
 Args    :


=cut

sub seed{
   my ($self,@args) = @_;

   my($dir,$id,$out);
   $dir = $self->_directory();
   $id  = $self->id();

   open(_SEED,"$dir/SEED") || $self->throw("For entry object [$id], got no valid directory for SEED alignment [$dir/SEED] $!");
   
   $out = Bio::Pfam::AlignPfam->new();
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
#   print "SOURCE: $value \n";
   if( defined $value) {
       $self->{'source'} = $value;
   }  else {
       $self->_before_annotation_hook('source');
   }
#    print "RET SOURCE: " .$self->{'source'} . " \n";
   return $self->{'source'};

}



sub ls_kappa{
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'ls_kappa'} = $value;
   }  else {
       $self->_before_annotation_hook('ls_kappa');
   }
    
   return $self->{'ls_kappa'};

}

sub ls_mu{
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'ls_mu'} = $value;
   }  else {
       $self->_before_annotation_hook('ls_mu');
   }
    
   return $self->{'ls_mu'};

}


sub fs_kappa{
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'fs_kappa'} = $value;
   }  else {
       $self->_before_annotation_hook('fs_kappa');
   }
    
   return $self->{'fs_kappa'};

}

sub fs_mu{
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'fs_mu'} = $value;
   }  else {
       $self->_before_annotation_hook('fs_mu');
   }
    
   return $self->{'fs_mu'};

}

sub comment{
   my ($self,$value) = @_;
 #  print "COMMENT: $value \n";
   if( defined $value) {
       $self->{'comment'} = $value;
   }  else {
       $self->_before_annotation_hook('comment');
   }
 #   print "RETURN: " .$self->{'comment'} . " \n";
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
       $value =~ s/;//g; 
       my($ls_sequence_TC, $ls_domain_TC,$fs_sequence_TC, $fs_domain_TC) = split (/\s+/, $value);
       $self->ls_sequence_trusted_cutoff($ls_sequence_TC);
       $self->ls_domain_trusted_cutoff($ls_domain_TC);
       $self->fs_sequence_trusted_cutoff($fs_sequence_TC);
       $self->fs_domain_trusted_cutoff($fs_domain_TC);
   }  else {
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
       
   }  else {
       $self->_before_annotation_hook('ls_sequence_trusted');
   }
   return $self->{'ls_sequence_trusted'};
}


sub ls_domain_trusted_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'ls_domain_trusted'} = $value;
   }  else {
       $self->_before_annotation_hook('ls_domain_trusted');
   }
   return $self->{'ls_domain_trusted'};
}

sub fs_sequence_trusted_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'fs_sequence_trusted'} = $value;
   }  else {
       $self->_before_annotation_hook('fs_sequence_trusted');
   }
   return $self->{'fs_sequence_trusted'};

}


sub fs_domain_trusted_cutoff {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'fs_domain_trusted'} = $value;
   }  else {
       $self->_before_annotation_hook('fs_domain_trusted');
   }
   return $self->{'fs_domain_trusted'};

}

#/ end new TRUSTED PARAMS


######### BUILD METHODS 


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




###### /end of build methods


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
   @hname = ( "HMM_ls", "HMM_fs" );
   $id  = $self->id();

   if($self->_loaded()) {
       $self->warn("Reloading annotation without indicating deloaded object.");
   }
   open(_SOURCE,"$dir/$fname") || $self->throw("For entry object [$id], got no valid directory for $fname [$dir/$fname] $!");
   
   $self->ann(Bio::Annotation::Collection->new());
   $self->_read_std_desc(\*_SOURCE,$self->{'_rcs_ann'});
   close(_SOURCE) || $self->throw("Could not close [$id] DESC in reading annotation");
   
   $self->_loaded(1);

   if (! $self->each_build_line and ! $self->is_dead) {
   
       foreach my $hname ( @hname ) { 
	   open(_HMM,"$dir/$hname") || $self->throw("For entry object [$id], got no valid directory for $hname [$dir/$hname] $!");

	   # The following strange syntax, using $l instrad of $_, avoids an
	   # unexplainable memory leak. Wierd.


	   while(1) {
	     my $l = <_HMM>;
	     $l =~ /^COM\s+(.*?)\s*$/ && do {
	       $str = $1;
	       $str =~ s/\/.*\/HMM/HMM/g;
	       $self->add_build_line($str);
	       next;
	     };
	     
	     $l =~ /^LENG\s+(\d+)$/ && do {
	       $self->model_length($1);
	       next;
	     };
	     
	     
	     $l =~ /^EVD\s+(\S+)\s+(\S+)/ && do {
	       my $mu = $1;
	       my $lambda = $2;
	       if ($hname =~ /ls/i) {
		 $self->ls_mu($mu);
		 $self->ls_kappa($lambda);
	       } else {
		 $self->fs_mu($mu);
		 $self->fs_kappa($lambda);
	       }
	       next;
	     };
	     
	     $l =~ /^HMM\s+/ && do {
	       last;
	     };
	     
	   }
	   close(_HMM) || $self->throw("Could not close [$id] $hname in reading annotation");
       }
   }
 


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


=head2 description

 Title   : description
 Usage   : $self->description($newval)
 Function: 
 Example : 
 Returns : value of description
 Args    : newvalue (optional)


=cut
sub description {
   my ($self,$value) = @_;

   if( defined $value) {
       $self->{'description'} = $value;
   } 
  
   return $self->{'description'};
}
