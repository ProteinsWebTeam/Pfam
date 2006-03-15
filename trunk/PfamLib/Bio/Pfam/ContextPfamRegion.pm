
#
# BioPerl module for Bio::Pfam::PfamRegion
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
# Copyright Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

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


package Bio::Pfam::ContextPfamRegion;
use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use strict;

use Bio::Pfam::AnnotatedRegion;

@ISA = qw(Bio::Pfam::AnnotatedRegion);

sub new {
  my($class, %params ) = @_;

  my( $acc, $id, $sentence_score, $contribution_to_sentence_score, $based_pfam_thres, $based_prob_thres, $length, $domain_score);

  $acc = ($params{'-PFAM_ACCESSION'}||$params{'-pfam_accession'});
  $id = ($params{'-PFAM_ID'}||$params{'-pfam_id'});
  $sentence_score = ($params{'-SENTENCE_SCORE'}||$params{'-sentence_score'});
  $contribution_to_sentence_score = ($params{'-CONTRIBUTION_TO_SENTENCE_SCORE'}||$params{'-contribution_to_sentence_score'});
  $based_pfam_thres = ($params{'-BASED_PFAM_THRES'}||$params{'-based_pfam_thres'});
  $based_prob_thres = ($params{'-BASED_PROB_THRES'}||$params{'-based_prob_thres'});
  $domain_score = ($params{'-DOMAIN_SCORE'}||$params{'-domain_score'});
  $length = ($params{'-LENGTH'}||$params{'-length'});
  my $type = ($params{'-TYPE'}||$params{'-type'});

  my $self = $class->SUPER::new( %params );
  $self->accession( $acc );
  $self->id( $id );
  $self->sentence_score( $sentence_score );
  $self->contribution_to_sentence_score( $contribution_to_sentence_score );
  $self->based_pfam_thres( $based_pfam_thres );
  $self->based_prob_thres( $based_prob_thres );
  $self->domain_score($domain_score);
  $self->type($type) if ($type);
  
  return $self; # success - we hope!
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
       $self->{'PfamReg_acc'} = $value;
   }
   return $self->{'PfamReg_acc'};
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
       $self->{'PfamReg_id'} = $value;
   }
   return $self->{'PfamReg_id'};
}


=head2 sentence_score

 Title   : sentence_score
 Usage   : 
    $reg->sentence_score(); # or ...
    $reg->sentence_score(15 );
 Function: For setting and getting the start point of the 

=cut

sub sentence_score {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'PfamReg_sentence_score'} = $value;
   }
   return $self->{'PfamReg_sentence_score'};
}


=head2 contribution_to_sentence_score

 Title   : contribution_to_sentence_score
 Usage   : 
    $reg->contribution_to_sentence_score(); # or ...
    $reg->contribution_to_sentence_score(15 );
 Function: For setting and getting the start point of the 

=cut

sub  contribution_to_sentence_score{
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'PfamReg_contribution_to_sentence_score'} = $value;
   }
   return $self->{'PfamReg_contribution_to_sentence_score'};
}

=head2 based_pfam_thres

 Title   : 
 Usage   : 
    $reg->based_pfam_thres(); # or ...
    $reg->based_pfam_thres(15 );
 Function: For setting and getting the start point of the 

=cut

sub  based_pfam_thres{
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'PfamReg_based_pfam_thres'} = $value;
   }
   return $self->{'PfamReg_based_pfam_thres'};
}


=head2 based_prob_thres

 Title   : 
 Usage   : 
    $reg->based_prob_thres(); # or ...
    $reg->based_prob_thres(15 );
 Function: For setting and getting the start point of the 

=cut

sub  based_prob_thres{
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'PfamReg_based_prob_thres'} = $value;
   }
   return $self->{'PfamReg_based_prob_thres'};
}


=head2 domain_score

 Title   : 
 Usage   : 
    $reg->domain_score(); # or ...
    $reg->domain_score15 );
 Function: For setting and getting the start point of the 

=cut

sub  domain_score{
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'PfamReg_domain_score'} = $value;
   }
   return $self->{'PfamReg_domain_score'};
}


sub type{
    my ($self, $value) = @_;
    
    if (defined $value) {
	$self->{'type'} = $value;
    }
    return $self->{'type'};
}
