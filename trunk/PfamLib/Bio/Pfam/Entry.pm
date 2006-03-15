

#
# BioPerl module for Bio::Pfam::Entry
#
# Cared for by Ewan Birney <pfam@sanger.ac.uk>
#
# Copyright Ewan Birney
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::Entry - Abstract Class for entry

=head1 SYNOPSIS
  
  #getting an Entry using the default database on site

  $db = &Bio::Pfam::default_db();
  $en = $db->get_entry_by_name('pkinase');

  $acc = $en->acc();
  $full = $en->full();

  print "Family accession is $acc\n";
  print "Alignment (Mul format)\n";
  $full->write_Pfam(\*STDOUT);

  # many other methods available

=head1 DESCRIPTION

Entry is the abstract class for Pfam entries. Each database will supply
its own concrete class. The functions here therefore the things you
can expect from both Pfam-A entries and Pfam-A entries

     #Pfam information methods - returns scalars
     $id    = $en->id();    # Pfam id
     $acc   = $en->acc();   # Pfam accession
     # author, alignment method, tc, nc ga also available     

     #sub object methods
     $align = $en->full();  # get full alignment object
     $ann   = $en->ann();   # get annotation object
     @nse   = $en->name_start_end('FULL'); # name/start-end scalars

     # handy output
     $en->write_std_desc(\*STDOUT); 

As long as you stick to these methods (and generally methods in this
file which do not start with an _ ) the script will be workable with 
different implementations of the database. However, each database
will implement both Pfam-A entries (EntryA) and Pfam-B entries (EntryB)
and these have slightly different interfaces. Methods defined in this 
object are common to both EntryA and EntryB

=head1 DEVELOPER INFORMATION

This file principly only defines abstract methods which are
then implemented by specific concrete classes, such as Entry_RCS.
A small number of concrete methods are provided in this class.

   write_std_desc  - writes a 'standard' desc file
   write_stockholm_ann - writes annotation suitable for a stockholm file
   _read_std_desc  - reads a standard desc file.

Notice that the read method is considered internal. It is in this
class as we expect that a number of different implementations will
want to use it (web and rcs for example). However, it should not
be called directly by scripts or client modules. (calling it directly
basically breaks the encapsulation of this system).

=head2 DESC file standard order

   Compulsory information (+ PI) (Non Annotation)

   Annotation Object (in this order!)
       References - RC, RM, RT, RA, RL
       Database Link - DC, DR,
       General Comment, CC

=head1 CONTACT

contact pfam@sanger.ac.uk for problems with this module

=head1 APPENDIX

The rest of the documentation details each of the object
methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...


package Bio::Pfam::Entry;
use vars qw($AUTOLOAD @ISA);
use strict;

# Object preamble - inheriets from Bio::Root::Object

use Bio::Pfam::Root;

@ISA = qw(Bio::Pfam::Root);

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
   $self->throw("Abstract method, Bio::Pfam::Entry::acc - should be filled by subclass");
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
   $self->throw("Abstract method, Bio::Pfam::Entry::id - should be filled by subclass");

   
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
   
   $self->throw("Abstract method, Bio::Pfam::Entry::author - should be filled by subclass");

}

=head2 ann

 Title   : ann
 Usage   : $self->ann($newval)
 Function: 
 Example : 
 Returns : Annotation object
 Args    : 


=cut

sub ann{
   my ($self,$value) = @_;

   $self->throw("Abstract method, Bio::Pfam::Entry::ann - should be filled by subclass");
     
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

   $self->throw("Abstract method, Bio::Pfam::Entry::full - should be filled by subclass");

}



=head2 annotated_regions

 Title   : 
 Usage   : @regs = $self->annotated_regions
 Function:
 Example :
 Returns : A list of AnnotatedRegion for the entry


=cut

sub annotated_regions {
   my ($self, $value) = @_;

   $self->throw("Abstract method, Bio::Pfam::Entry::annotated_regions - should be filled by subclass");
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
   my ($self,@args) = @_;

   $self->throw("Abstract method, Bio::Pfam::Entry::name_start_end - should be filled by subclass");

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

   $self->throw("Abstract method, Bio::Pfam::Entry::num_seqs_in_full - should be filled by subclass");
}


=head2 write_std_desc

 Title   : write_std_desc
 Usage   :
 Function:
 Example :
 Returns : 
 Args    :


=cut

sub write_std_desc{
   my ($self,$file) = @_;

   $self->write_std_desc_tag("","   ",$file);
}



=head2 write_std_desc_tag

 Title   : write_std_desc_tag
 Usage   : $en->write_std_desc_tag($before,$after,\*FILE);
 Function: produces a standard description file from the entry
           with 

           $before<tag>$after<text>

           so standard desc is write_std_desc_tag('',"   ",$file);

           and stockholm is        write_std_desc_tag("#="," ",$file);
 Example :
 Returns : nothing
 Args    : string, string, Filehandle glob or object + optional boolean


=cut

sub write_std_desc_tag {
    my ($self, $before, $after, $file) = @_;

    $self->throw("Abstract method, Bio::Pfam::Entry::write_std_desc_tag - should be filled by subclass");

}

=head2 write_stockholm_ann

 Title   : write_stockholm_ann
 Usage   : $en->write_stockholm_ann(\*STDOUT)
 Function: writes a stockholm file with = annotation lines
 Example :
 Returns : 
 Args    :


=cut

sub write_stockholm_ann{
   my ($self,$file) = @_;

   $self->write_std_desc_tag("#=GF ","   ",$file);
   return;

}




=head2 _read_std_desc

 Title   : _read_std_desc
 Usage   : $self->_read_std_desc(\*FILE)
 Function: This reads a 'standard' Pfam desc file
           with two letter codes in

           This function is in the abstract entry class as
           we expect different implementation to want to reuse
           it. *However* it should not be called directly
           by a script - use the ann() method instead.
 Example : 
 Returns : 
 Args    :


=cut

sub _read_std_desc{
   my ($self,$file,$ann) = @_;

   $self->throw("Abstract method Bio::Pfam::Entry::read_sts_desc, should be implemented by sub-class");
}




