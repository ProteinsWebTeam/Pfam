
#
# BioPerl module for Bio::Pfam::EntryB_Impl
#
# Cared for by Kevin Howe<pfam@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::EntryB_Impl - DESCRIPTION of Object

=head1 SYNOPSIS

Give standard usage here

=head1 DESCRIPTION

Describe the object here

=head1 CONTACT

Describe contact details here

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...


package Bio::Pfam::EntryB_Impl;
use vars qw($AUTOLOAD @ISA);
use strict;

# Object preamble - inheriets from Bio::Root::Object

use Bio::Pfam::AlignPfam;
use Bio::Pfam::EntryB;

@ISA = qw(Bio::Pfam::EntryB);


sub new {
  my($class,@args) = @_;
  my $self = $class->SUPER::new(@args);
  return $self;
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
   } else {
       $self->_before_annotation_hook('num_seqs_in_full');
   }
   
   return $self->{'num_seqs_in_full'};
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
   my($data,$id,$out);

   $data = $self->_datasource();

   open(_DATA, "$data") || $self->throw("For entry object [$id], got no valid directory for alignment [$data] $!");

   # we need to place the file pointer at the alignment part of the Entry
   while (<_DATA>) {
       /^SQ/ && last;
   }
   if (eof(_DATA)) {
       $self->throw("This Pfam-B entry had no SQ line [$data]");
   }

   $out = Bio::Pfam::AlignPfam->new();
   $out->read_stockholm(\*_DATA);

   close(_DATA);

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
	if( $obj->_loaded() == 0 ) {
	    $obj->_load_annotation();
	}
    }

    return $obj->{'_web_ann'};

}



sub annotated_regions {
   my ($self) = @_;
   my ($com, $acc, $id, @list);

   $com = $self->_datasource();
       
   open(_COM, "$com") || $self->throw("Could not open [$com]");
   while(<_COM>) {
       /^ID\s+(\S+)/ && do {
	   $id = $1;
	   next;
       };
       /^AC\s+(\S+)/ && do {
	   $acc = $1;
	   next;
       };
       /^(\S+)\/(\d+)\-(\d+)\s+/ && do {
	   push @list, Bio::Pfam::PfamRegion->new('-PFAM_ACCESSION' => $acc,
						  '-PFAM_ID' => $id,
						  '-SEQ_ID' => $1,
						  '-FROM' => $2, 
						  '-TO' => $3
						  );
	   next;
       };
   }
   close(_COM) || $self->throw("Could not close command [$com]");

   return @list;
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
   my ($self) = @_;

   my($data,$id,$out,$name, $acc, @list);

   $data = $self->_datasource();
   $acc  = $self->acc();

   open(_ALIGN, "$data") || $self->throw("For entry object [$id], got no valid source for alignment [$data] $!");

   while(<_ALIGN>) {
       /^(\S+\/\d+\-\d+)/ && push(@list, $1);
   }

   close(_ALIGN) || $self->throw("Could not close alignment file handle [$data]");

   return @list;
}


=head2 _datasource

 Title   : _datasource
 Usage   : $obj->_datasource($newval)
 Function: file or command where entry data is stored 
 Returns : value of _datasource
 Args    : newvalue (optional)


=cut

sub _datasource{
   my ($self,$value) = @_;
   if( defined $value) {
      $self->{'_datasource'} = $value;
    }
    return $self->{'_datasource'};

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

   if ($type eq 'acc' or $type eq 'id') {
       return 1;
   }

   if( $self->_loaded() == 1 ) {
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
   my($data);

   if( $self->_loaded() == 1) {
       $self->warn("Reloading annotation without indicating deloaded object.");
   }

   $data = $self->_datasource();

   open(_DESC,"$data") || $self->throw("For entry object, got no valid directory for desc file [$data]");

   $self->ann(new Bio::Annotation::Collection);
   $self->_read_std_desc(\*_DESC,$self->{'_web_ann'});

   close(_DESC) || $self->throw("Could not close DESC in reading annotation");
   $self->_loaded(1);
   return 1;
     
}






