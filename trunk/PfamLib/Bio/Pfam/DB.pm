

#
# BioPerl module for PfamDB
#
# Cared for by Ewan Birney, Pfam <pfam@sanger.ac.uk>
#
# Copyright Ewan Birney, Pfam
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::PfamDB - Bio::PfamDB is 'root' object to a Pfam database

=head1 SYNOPSIS

    use Bio::PfamDB;

    $db = new Bio::PfamDB(); # builds from defaults
 
    @list = $db->get_allacc('ALPHA'); #gets accession 
                                      #numbers alphabetically
	                              #by id

=head1 DESCRIPTION

Bio::PfamDB holds the information for a Pfam database, whether ontop of a live database
(such as the current RCS system) or an indexed, read-only release. 

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

This module was developed originally by Ewan Birney <birney@sanger.ac.uk> but is 
envisaged to outlast his stay at the Sanger centre. The most stable contact email is
probably pfam@sanger.ac.uk

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...


package Bio::Pfam::DB;
use vars qw($AUTOLOAD @ISA);
use strict;

use Bio::Pfam::Root;

@ISA = qw(Bio::Pfam::Root);

# new is superfluous unless we actually initialise something here
sub new {
    my( $class, @args ) = @_;
    my $self = $class->SUPER::new( @args );
    return $self;
}


=head2 id2acc

 Title   : id2acc
 Usage   : $acc = $db->id2acc("pkinase"); 
 Function: converts an id to an accession. Only works for
	   current ids - does not work for previous ids
 Returns : string of the accession, or undef if no accession
 Args    :

=cut

sub id2acc{
   my ($self,@args) = @_;
   
   $self->throw("Not implemented yet");
}

=head2 acc2id

 Title   : acc2id
 Usage   : $id = $db->acc2id("PF00001");
 Function: converts an accession to an id
 Returns : the id, or undef if no id for that accession
 Args    :

=cut

sub acc2id{
   my ($self,@args) = @_;

   $self->throw("Defaulted to underlying Pfam::DB module. Bad news");
}


=head2 get_dead_accs

 Title   : get_dead_accs
 Usage   : @list = $db->get_dead_accs('ALPHA');
 Function: Gets a list of accession numbers of dead familes in current db
 Returns : a list of accession numbers
 Args    : Either 'ALPHA' or 'DATE' for the sort
           by alphabetical order of id or date.

=cut

sub get_dead_accs{
   my ($self,@args) = @_;

   $self->throw("Defaulted to underlying Pfam::DB module. Bad news");
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

   $self->throw("Defaulted to underlying Pfam::DB module. Bad news");

}

=head2 get_random_acc

 Title   : get_random_acc
 Usage   : @list = $db->get_random_acc($number);
 Function: Gets $number accession numbers randomly in the current db
 Returns : list of accession numbers

=cut

sub get_random_acc{
   my ($self,$number) = @_;
   srand(time^($$+($$<<15)));
   my (@list,@outputlist,$listlen);

   @list = $self->get_allacc('ALPHA');
   $listlen=@list;

   while ($number){
     push(@outputlist,splice(@list,int(rand($listlen)),1));

     $listlen--;
     $number--;
   }
   return (@outputlist);
}



=head2 get_Entry_by_id

 Title   : get_Entry_by_id
 Usage   : $en = $db->get_Entry_by_id('pkinase');
 Function: get a Bio::Pfam::Entry object by id
 Returns : Bio::Pfam::Entry object
 Args    : string of the id of the family

=cut

sub get_Entry_by_id{
   my ($self,@args) = @_;

   $self->throw("Bio::Pfam::DB::get_Entry_by_id is deprecated. Use get_EntryA_by_id");
}




=head2 get_EntryA_by_id

 Title   : get_EntryA_by_id
 Usage   : $en = $db->get_EntryA_by_id('pkinase');
 Function: get a Bio::Pfam::EntryA object by id
 Returns : Bio::Pfam::EntryA object
 Args    : string of the id of the family


=cut

sub get_EntryA_by_id{
   my ($self,@args) = @_;

   $self->throw("Defaulted to underlying Pfam::DB module. Bad news");
}




=head2 get_EntryB_by_id

 Title   : get_EntryB_by_id
 Usage   : $en = $db->get_EntryB_by_id('Pfam-B_101');
 Function: get a Bio::Pfam::EntryB object by id
 Returns : Bio::Pfam::EntryB object
 Args    : string of the id of the family


=cut

sub get_EntryB_by_id{
   my ($self,@args) = @_;

   $self->throw("Defaulted to underlying Pfam::DB module. Bad news");
}





=head2 get_Entry_by_acc

 Title   : get_Entry_by_acc
 Usage   : $en = $db->get_Entry_by_acc('PF00076');
 Function: get a Bio::Pfam::Entry object by acc
 Returns : Bio::Pfam::Entry object
 Args    : string of the accession number of the family


=cut

sub get_Entry_by_acc{
   my ($self,@args) = @_;

   $self->throw("Bio::Pfam::DB::get_Entry_by_acc is deprecated. Use get_EntryA_by_acc");
}



=head2 get_EntryA_by_acc

 Title   : get_EntryA_by_acc
 Usage   : $en = $db->get_EntryA_by_acc('PF00076');
 Function: get a Bio::Pfam::Entry object by acc
 Returns : Bio::Pfam::Entry object
 Args    : string of the accession number of the family


=cut

sub get_EntryA_by_acc {
   my ($self,@args) = @_;

   $self->throw("get_EntryA_by_acc defaulted to underlying Pfam::DB module. Bad news");
}




=head2 get_EntryB_by_acc

 Title   : get_EntryB_by_acc
 Usage   : $en = $db->get_Entry_by_acc('PB000076');
 Function: get a Bio::Pfam::Entry object by acc
 Returns : Bio::Pfam::Entry object
 Args    : string of the accession number of the family


=cut

sub get_EntryB_by_acc {
   my ($self,@args) = @_;

   $self->throw("get_EntryB_by_acc defaulted to underlying Pfam::DB module. Bad news");
}




=head2 get_AnnotSeqs

 Title   : get_AnnotSeqs
 Usage   : ($annot1, $annot2) = $db->get_AnnotSeqs( [$id1, $id2], $type1, $type2 );
 Function: gets Annotated sequencea from underlying swisspfam database
 Returns : Bio::Pfam::AnnotatedSequence (list of) 
 Args    : 
    A pfamseq identifier (ref to a list of)
    A list of region types needed
 Notes   : 
    1. The AnnotatedSequence returned has no enclosed sequence,
    just annotation. If the sequence is required, use 
    get_Seq_pfamseq to get a Bio::Pfam::SeqPfam and insert it into
    the AnnotatedSequence method using 
    AnnotatedSequence->sequence().

    2. Region-type items can be:
        'seed'
        'full'
        'pfamb'
        'other'
    When the list is empty, all regions are returned. If not, then 
    if the specified region types are availalble they will be 
    returned, but the types specified will not be returned

=cut

sub get_AnnotSeqs{
   my ($self, @args) = @_;

   $self->throw("Defaulted to underlying Pfam::DB module. Bad news");
}



=head2 get_Seq_pfamseq

 Title   : get_Seq_pfamseq
 Usage   : $seq = $db->get_Seq_pfamseq
 Function: gets a Bio::Pfam::SeqPfam object from the underlying database
 Returns : a newly made Bio::Pfam::SeqPfam object
 Args    :


=cut

sub get_Seq_pfamseq{
   my ($self,@args) = @_;

   $self->throw("Defaulted to underlying Pfam::DB module. Bad news");

}



=head2 pfamseq_stats

 Title   : pfamseq_stats
 Usage   : ($residues,$sequences) = $db->pfamseq_stats()
 Function: Calculate statistics for underlying pfamseq database
           
 Returns : An array of number of residues and sequences
         : in pfamseq
 Args    :


=cut

sub pfamseq_stats {
   my ($self,@args) = @_;

   $self->throw("Defaulted to underlying Pfam::DB module. Bad news");

}

