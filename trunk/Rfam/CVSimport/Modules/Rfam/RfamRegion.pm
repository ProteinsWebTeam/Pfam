


=head1 NAME

Bio::Pfam::PfamRegion - Representation of a protein sequence domain in Pfam

=head1 SYNOPSIS

    use Bio::Pfam::PfamRegion;

    $pfamAreg = new Bio::Pfam::PfamRegion( '-PFAM_ACCESSION' => $acc,
					   '-PFAM_ID' => $id,
					   '-SEQ_ID' => $seq_id,
					   '-FROM' => $start,
					   '-TO' => $end,
					   '-MODEL_FROM' => $model_from,
					   '-MODEL_END' => $model_end,
					   '-BITS' => $bits,
					   '-EVALUE' => $evalue,
					   '-ANNOTATION' => $annotationRef);

    $pfamBreg = new Bio::Pfam::PfamRegion( '-PFAM_ACCESSION' => 'PB000111',
					   '-PFAM_ID' => $id,
					   '-SEQ_ID' => $seq_id,
					   '-FROM' => $start,
					   '-TO' => $end,
					   '-ANNOTATION' => $annotationRef);


=head1 DESCRIPTION

This object stores the details for a Pfam domain. It is derived from the
AnnotatedRegion class. Information that must be given is the name and Pfam 
accession number of the domain, the extent of the domain (in terms of start and end
indices), and an annotation for the domain.

All PfamRegions are described by this object, including those from full and seed
alignments and Pfam-B regions. This means that certain fields will only be
pertinent for certain types of Pfam region. Those fields common to all PfamRegions are:

accession
id
seq_name
from
to

Other fields should be checked for defined-ness for use.

=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...


package Rfam::RfamRegion;
use vars qw($AUTOLOAD @ISA);
use strict;


use Rfam::AnnotatedRegion;

@ISA = qw(Rfam::AnnotatedRegion);

sub new {
  my $caller = shift;
  my $class  = ref( $caller ) || $caller;
  my %params = @_;
  my $self = $class->SUPER::new(%params);

  my( $acc, $id, $mod_st, $mod_en, $bits, $length, $rfamseq_id, $auto_rfam);

  $acc = ($params{'-RFAM_ACCESSION'}||$params{'-rfam_accession'});
  $id = ($params{'-RFAM_ID'}||$params{'-rfam_id'});
  $mod_st = ($params{'-MODEL_FROM'}||$params{'-model_from'});
  $mod_en = ($params{'-MODEL_TO'}||$params{'-model_to'});
  $bits = ($params{'-BITS'}||$params{'-bits'});
  $length = ($params{'-LENGTH'}||$params{'-length'});
  $rfamseq_id = ($params{'-SEQ_ID'}||$params{'-seq_id'});
  $auto_rfam = ($params{'-AUTO_RFAM'}||$params{'-auto_rfam'});


  my $make = $self->SUPER::new( %params );
  $self->accession( $acc );
  $self->id( $id );
  $self->model_from( $mod_st );
  $self->model_to( $mod_en );
  $self->bits_score( $bits );
  $self->rfamseq_id($rfamseq_id);
  $self->auto_rfam($auto_rfam);

  return $self;
}





=head2 accession

 Title   : accession
 Usage   : 
    $dom->accession(); # or ...
    $dom->accession( 123 );
 Function: For setting and getting the ACCESSION field in the object

=cut

sub accession{
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'RFAM_ACC'} = $value;
   }
   return $self->{'RFAM_ACC'};
}




=head2 id

 Title   : id
 Usage   : 
    $dom->id(); # or ...
    $dom->id( "helloSir" );
 Function: For setting and getting the ID field in the object

=cut

sub id{
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'RFAM_ID'} = $value;
   }
   return $self->{'RFAM_ID'};
}


=head2 model_from

 Title   : model_from
 Usage   : 
    $reg->model_from(); # or ...
    $reg->model_from(15 );
 Function: For setting and getting the start point of the model

=cut

sub model_from {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'MODEL_FROM'} = $value;
   }
   return $self->{'MODEL_FROM'};
}




=head2 model_to

 Title   : model_to
 Usage   : 
    $reg->model_to(); # or ...
    $reg->model_to(15 );
 Function: For setting and getting the end point of the model

=cut

sub model_to {
   my ($self, $value) = @_;


   if (defined $value) {
       $self->{'MODEL_TO'} = $value;
   }
   return $self->{'MODEL_TO'};
}





=head2 bits_score

 Title   : bits_score
 Usage   : 
    $reg->bits_score(); # or ...
    $reg->bits_score(15 );
 Function: For setting and getting bits score of this PfamRegion with respect to the model

=cut

sub bits_score {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'BITS'} = $value;
   }
   return $self->{'BITS'};
}



sub rfamseq_id {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'RFAMSEQ_ID'} = $value;
   }
   return $self->{'RFAMSEQ_ID'};
}




sub auto_rfam {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'AUTO_RFAM'} = $value;
   }
   return $self->{'AUTO_RFAM'};
}


1;
