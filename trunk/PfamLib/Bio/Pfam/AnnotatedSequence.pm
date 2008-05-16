
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

# Author: rdf

package Bio::Pfam::AnnotatedSequence;
use vars qw($AUTOLOAD @ISA);
use strict;
use warnings;

use Bio::Pfam::Root;

@ISA = qw(Bio::Pfam::Root);

sub new {
  my($class, %params) = @_;

  my $self = $class->SUPER::new(%params);

  $self->{ 'annSeq_sequence' } = undef;
  $self->{ 'annSeq_RegionList' } = [];
  $self->{ 'annSeq_FeatureList' } = [];
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


=head2 addFeature

 Title   : addFeature
 Usage   : @regions = $annSeq->addFeature(Bio::SeqFeature::Generic-new( -start => 10, 
									-end => 110, 
									-primary => 'disulphide',
									-source_tag => 'uniprot',
									-display_name => 'S=S'));
 Function: A way of adding sequence features from the AnnotatedSequence
 Returns : Nothing
 Args    : A SeqFeature object

=cut

sub addFeature{
   my ($self, $feature) = @_;
   push(@{$self->{ 'annSeq_FeatureList' }}, $feature);
}

=head2 eachFeature

 Title   : eachFeature
 Usage   : @regions = $annSeq->eachFeature();
 Function: A way of getting aby sequence features from the AnnotatedSequence
 Returns : A list of SeqFeature objects
 Args    : None

=cut

sub eachFeature{
   my ($self) = @_;
   return @{$self->{ 'annSeq_FeatureList' }};
}


=head2 id

 Title   : id
 Usage   : 
    $annSeq->id(); $annSeq->id( $theID );
 Function: 
  Sets or returns this AnnotatedSequence object's id field
  Note: The id of the sequence stored and the id of this object should be identical.
    The only reason for providing an id field in this object is that there may not
    be a sequence

 Returns : An identifier string
 Args    : An identifier string (optional)

=cut

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




=head2 length

 Title   : length
 Usage   : 
    $annSeq->length(); $annSeq->length( $thelength );
 Function: 
  Sets or returns this AnnotatedSequence object's length field
  Note: The length stored and the length of the stored sequecnes should be the same;
    The only reason for providing an id field in this object is that there may not
    be a sequence

 Returns : length of the sequence
 Args    : length of the sequence (optional)

=cut

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
       return $self->{'annSeq_sequence'}->length();
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




=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
