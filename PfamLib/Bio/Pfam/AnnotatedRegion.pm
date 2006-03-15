
#
# BioPerl module for Bio::Pfam::AnnotatedRegion
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::AnnotatedRegion - Object for the annotation of a specified region of a sequence

=head1 SYNOPSIS

Derive from this class to create a new kind of AnnotatedRegion. The three
core fields are -FROM, -TO, and -ANNOTATION (containing the start and end residue numbers
and a reference to an Annotation object):

    use Bio::Pfam::AnnotatedRegion;

    $reg = Bio::Pfam::AnnotatedRegion->new('-SEQ_ID' => $name,
					   '-FROM' => $from,
					   '-TO' => $to,
					   '-ANNOTATION' => annot);


=head1 DESCRIPTION

This object is the superclass of several different types of AnnotatedRegion,
including PfamRegion. The generic AnnotatedSequence class (which
encapsulates a sequence together with its annotation) will contain a list of
instances of AnnotatedRegion derivatives.

=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...


package Bio::Pfam::AnnotatedRegion;
use vars qw($AUTOLOAD @ISA);
use strict;

use Bio::Pfam::Root;

@ISA = qw(Bio::Pfam::Root);

sub new {
  my($class, %params) = @_;
  my( $start, $end, $seqname, $annot ) = 
      (
       ($params{'-FROM'}||$params{'-from'}),
       ($params{'-TO'}||$params{'-to'}),
       ($params{'-SEQ_ID'}||$params{'-seq_id'}),
       ($params{'-ANNOTATION'}||$params{'-annotation'})
      );

  my $self = $class->SUPER::new(%params);

  $self->{ 'reg_from' } = $start;
  $self->{ 'reg_to' } = $end;
  $self->{ 'reg_seqname' } = $seqname;
  $self->{ 'reg_annot' } = $annot;

  return $self;
}




=head2 annotation

 Title   : annotation
 Usage   : 
    $annReg->annotation(); # or ...
    $annReg->annotation( 123 );
 Function: For setting and getting the ANNOTATION field in the object

=cut

sub annotation{
   my ($self,$value) = @_;

   if (defined $value) {
       $self->{'reg_annot'} = $value;
   }
   return $self->{'reg_annot'};
}




=head2 from

 Title   : from
 Usage   : 
    $annReg->from(); # or ...
    $annReg->from( 123 );
 Function: For setting and getting the FROM field in the object

=cut

sub from{
   my ($self, $value) = @_;


   if (defined $value) {
       $self->{'reg_from'} = $value;
   }
   return $self->{'reg_from'};
}



=head2 to

 Title   : to
 Usage   : 
    $annReg->to(); # or ...
    $annReg->to( 123 );
 Function: For setting and getting the TO field in the object

=cut

sub to{
   my ($self, $value) = @_;


   if (defined $value) {
       $self->{'reg_to'} = $value;
   }
   return $self->{'reg_to'};
}



=head2 seq_name

 Title   : seq_name
 Usage   : 
    $annReg->seq_name(); # or ...
    $annReg->seq_name( 123 );
 Function: For setting and getting the sequence-name field in the object

=cut

sub seq_name {
   my ($self, $value) = @_;


   if (defined $value) {
       $self->{'reg_seqname'} = $value;
   }
   return $self->{'reg_seqname'};
}
