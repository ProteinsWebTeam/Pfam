
#
# BioPerl module for Bio::Pfam::AnnotatedSequence
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::AnnotatedSequence - Object encapsulating a sequence along with its annotation.

=head1 SYNOPSIS

    use Bio::Pfam::PfamAnnSeqFactory; # or annother kind of factory

    $fac = Bio::Pfam::PfamAnnSeqFactory->instance();

    $annSeq = $fac->createAnnotatedSequence(); 


=head1 DESCRIPTION

Bio::Pfam::AnnotatedSequence encapsulates sequence together with its 
annotation. Objects of this class are obtained not be creating them 
directly but by asking a single instance of an appropriate derivative of 
AnnSeqFactory for it. AnnotatedSequence objects can be
initialised in a variety of ways, and AnnSeqFactory contains
methos for each of these ways. One of these ways, from the results of
an HMM search, is shown in the synopsis.

Note that once an AnnotatedSequence object has been obtained, its 
annotation can be extended by adding further AnnotatedRegion records
by calling addAnnotatedRegion. The list of AnnotatedRegions can be 
obtained by eachAnnotatedRegion, although the caller should be aware 
that the list may contain instances of different derivatives of 
AnnotatedRegion.


=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...


package Rfam::AnnotatedSequence;
use vars qw($AUTOLOAD @ISA);
use strict;

# Object preamble - inherits from Bio::Root::Object

#use Bio::Root::Object;

#@ISA = qw(Bio::Root::Object Exporter);
#@EXPORT_OK = qw();
# new() is inherited from Bio::Root::Object

# _initialize is where the heavy stuff will happen when new is called

sub new {

    my $caller = shift;
    my $self = bless {}, ref($caller) || $caller;
    my %params = @_;
    $self->{ 'annSeq_sequence' } = undef;
    $self->{ 'annSeq_RegionList' } = [];

    return $self;

}







=head2 addAnnotatedRegion

 Title   : addAnnotatedRegion
 Usage   : 
    $annSeq->addAnnotatedRegion( Bio::Pfam::AnnotatedRegion->new('-FROM' => $start, 
								 '-TO' =>$end,
								 '-ANNOTATION' => $annotationRef ) );
 Function: Updates the AnnotatedSequence object with an additional annotated region
 Returns : 
 Args    : start index of region, end index of region, Annotation object reference

=cut

sub addAnnotatedRegion{
   my ($self, $reg) = @_;

   # simply create the Annotated Region and push it onto the list
 #  print "THE REG: $reg <P>";
   push @{$self->{ 'annSeq_RegionList' }}, $reg;
}




=head2 eachAnnotatedRegion

 Title   : eachAnnotatedRegion
 Usage   : @regions = $annSeq->eachAnnotatedRegion();
 Function: A way of getting the secondary annotation from the AnnotatedSequence
 Returns : A list of AnnotatedRegion references
 Args    : None

=cut

sub eachAnnotatedRegion{
   my ($self) = @_;

   return @{$self->{ 'annSeq_RegionList' }};
}







sub id{
   my ($self, $value) = @_;

   if (defined $value) {
       if (defined $self->{'annSeq_sequence'} ) {
	   $self->{'annSeq_sequence'}->id( $value );
       }
       else {
	   $self->{'annSeq_id'} = $value;
       }
   }

   if (defined $self->{'annSeq_sequence'} ) {
       return $self->{'annSeq_sequence'}->id();
   }
   else {
       return $self->{'annSeq_id'};
   }
}





sub length {
   my ($self, $value) = @_;

   if (defined $value) {
       if (defined $self->{'annSeq_sequence'} ) {
	   $self->throw("You cannot set the length of the enclosed Bio::Pfam::SeqPfam object");
       }
       else {
	   $self->{'annSeq_length'} = $value;
       }
   }

   if (defined $self->{'annSeq_sequence'} ) {
       return $self->{'annSeq_sequence'}->seq_len();
   }
   else {
       return $self->{'annSeq_length'};
   }
}





=head2 sequence

 Title   : sequence
 Usage   : 
    $domsq->sequence(); $domsq->sequence( $theSequence );
 Function: sets or returns this AnnotatedSequence objects sequence field
 Returns : ref to a Bio::Pfam::SeqPfam object
 Args    : A Bio::Pfam::SeqPfam reference (optional)

=cut

sub sequence{
   my ($self, $value) = @_;

   if (defined $value) {
       # check that the sequence has a start and end. This is important
       if (! defined $value->start() ) {
	   $value->start( '1' );
       }
       if (! defined $value->end() ) {
	   $value->end( $value->seq_len() );
       }

       $self->{'annSeq_sequence'} = $value;
   }
   return $self->{'annSeq_sequence'};
}




=head2 write

 Title   : write
 Usage   : $annSeq->write( \*STDOUT );
 Function: Spits out the contents of the object in standard fashion to the 
    given file handle
 Returns : 
 Args    : 
    The output file handle, 
    boolean, true if is required that the sequence be printed

=cut

sub write{
   my ($self, $file, $sequenceRequired) = @_;

   if ($sequenceRequired) {
       if (defined $self->sequence()) {
	   print $file $self->sequence()->out_fasta();
       }
       else {
	   print $file $self->id()." No sequence defined\n";
       }
   }
   foreach my $reg ( $self->eachAnnotatedRegion() )  {
       $reg->write( $file );
   }
}




1;
