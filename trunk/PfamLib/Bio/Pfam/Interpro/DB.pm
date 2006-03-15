

#
# BioPerl module for Bio::Interpro::DB
#
# Cared for by Kevin Howe, Pfam <pfam@sanger.ac.uk>
#
# Copyright Kevin Howe, Pfam
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::Interpro::DB - Bio::Pfam::Interpro::DB is 'root' object to an Intepro database


=head1 DESCRIPTION

Bio::Pfam::Interpro::DB holds the information for an Interpro database, whether ontop of a live database
(such as the live ORACLE system) or an indexed, read-only xml release. 

By having this abstraction, scripts become portable between fixed released and the current
database, and in addition this module insulates scripts from the underlying representation
of the database.

You will in fact never use any functionality written in this module- this provides only
the documentation of methods that are expected to be applied by any module inherieting
from it. The only 'thing' that this module does is throw exceptions if a function call
is not provided by another module.

As long as you use the functionality in this module for accessing the database you
should be completely insulated from the underlying location and storage of the database

=head1 CONTACT

This module was developed originally by Kevin Howe <birney@sanger.ac.uk> but is 
envisaged to outlast his stay at the Sanger centre. The most stable contact email is
probably pfam@sanger.ac.uk

=cut

package Bio::Pfam::Interpro::DB;
use vars qw($AUTOLOAD @ISA);
use strict;

# Object preamble - inheriets from Bio::Root::Object

use Bio::Pfam::Root;
use Bio::Pfam::Interpro::Entry;

@ISA = qw(Bio::Pfam::Root);

sub new {
  my($class,@args) = @_;
  my $self = $class->SUPER::new(@args);
  return $self;
}


=head2 id2acc

 Title   : id2acc
 Usage   : $acc = $db->id2acc("efhand"); 
 Function: converts an id to an accession. Only works for
	   current ids - does not work for previous ids
 Returns : string of the accession, or undef if no accession
 Args    :

=cut

sub id2acc{
   my ($self,@args) = @_;
   
   $self->throw("Defaulted to underlying Interpro::DB module. Bad news");
}


=head2 acc2id

 Title   : acc2id
 Usage   : $id = $db->acc2id("IPR000001");
 Function: converts an accession to an id
 Returns : the id, or undef if no id for that accession
 Args    :

=cut

sub acc2id{
   my ($self,@args) = @_;

   $self->throw("Defaulted to underlying Interpro::DB module. Bad news");
}


=head2 get_allacc

 Title   : get_allacc
 Usage   : @list = $db->get_allacc('ALPHA');
 Function: Gets a list of all accession numbers in the current db
 Returns : a list of accession numbers
 Args    : Either 'ALPHA' or 'DATE' for the sort
           by alphabetical order of id or date.

=cut

sub get_allacc{
   my ($self,@args) = @_;

   $self->throw("Defaulted to underlying Interpro::DB module. Bad news");

}



=head2 get_Entry_by_id

 Title   : get_Entry_by_id
 Usage   : $en = $db->get_Entry_by_id('pkinase');

 Function: get a Bio::Pfam::Interpro::Entry object by id
 Returns : Bio::Pfam::Entry object
 Args    : string of the id of the family

=cut

sub get_Entry_by_id{
   my ($self,$id) = @_;

   my $en = Bio::Pfam::Interpro::Entry->new();
   open (_TEMP, "getz -e '[interpro-nam:$id]' |");
   $en->_read_std_entry(\*_TEMP);
   if (not $en->acc()) {
       $self->throw("There is no Interpro entry for $id");
   }
   
   return $en;
}




=head2 get_Entry_by_acc

 Title   : get_Entry_by_acc
 Usage   : $en = $db->get_Entry_by_acc('IPR000111');
 Function: get a Bio::Pfam::Entry object by acc
 Returns : Bio::Pfam::Entry object
 Args    : string of the accession number of the family


=cut

sub get_Entry_by_acc{
   my ($self,$acc) = @_;
   my $en = Bio::Pfam::Interpro::Entry->new();
   open (_TEMP, "getz -e '[interpro-acc:$acc]' |");
	
   $en->_read_std_entry(\*_TEMP);
   if (not $en->acc()) {
       $self->throw("There is no Interpro entry for $acc");
   }
   
   return $en;
}


=head2 get_all_Proteins

 Title   : get_all_Entries
 Usage   : @list = $db->get_all_Entries();
 Function: Gets a list of all Entry objects in the current db
 Returns : a reference to a list of Entry objects
 Args    : none

=cut


sub get_all_Proteins {
  my ($self,@args) = @_;
  $self->throw("Defaulted to underlying Interpro::DB module. Bad news");
}




return 1 ; # end of module
