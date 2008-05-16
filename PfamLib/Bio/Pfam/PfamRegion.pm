
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

# $Author: jt6 $

# Let the code begin...


package Bio::Pfam::PfamRegion;
use vars qw($AUTOLOAD @ISA);
use strict;
use warnings;

use Bio::Pfam::AnnotatedRegion;
@ISA = qw(Bio::Pfam::AnnotatedRegion);


sub new {
  my( $class, %params ) = @_;

  my $mode = ($params{'-MODE'}||$params{'-mode'});
  my $acc = ($params{'-PFAM_ACCESSION'}||$params{'-pfam_accession'});
  my $id = ($params{'-PFAM_ID'}||$params{'-pfam_id'});
  my $mod_st = ($params{'-MODEL_FROM'}||$params{'-model_from'});
  my $mod_en = ($params{'-MODEL_TO'}||$params{'-model_to'});
  my $bits = ($params{'-BITS'}||$params{'-bits'});
  my $evalue = ($params{'-EVALUE'}||$params{'-evalue'});
  my $domain_bits = ($params{'-DOMAIN_BITS'}||$params{'-domain_bits'});
  my $domain_evalue = ($params{'-DOMAIN_EVALUE'}||$params{'-domain_evalue'});
  my $sequence_bits = ($params{'-SEQUENCE_BITS'}||$params{'-sequence_bits'});
  my $sequence_evalue  = ($params{'-SEQUENCE_EVALUE'}||$params{'-sequence_evalue'});
  my $is_significant = ($params{'-IS_SIGNIFICANT'}||$params{'-is_significant'});
  my $in_full = ($params{'-IN_FULL'}||$params{'-in_full'});
  my $length = ($params{'-LENGTH'}||$params{'-length'});
  my $type = ($params{'-TYPE'}||$params{'-type'});
  my $pfamseq_id = ($params{'-SEQ_ID'}||$params{'-seq_id'});
  my $pfamseq_acc = ($params{'-SEQ_ACC'}||$params{'-seq_acc'});
  my $pfamseq_desc = ($params{'-SEQ_DESC'}||$params{'-seq_desc'});
  my $tree_order = ($params{'-TREE_ORDER'}||$params{'-tree_order'});
  my $auto_pfamA = ($params{'-AUTO_PFAMA'}||$params{'-auto_pfamA'});
  my $mod_len = ($params{'-MODEL_LENGTH'}||$params{'-model_length'});
  my $region = ($params{'-REGION'}||$params{'-region'});

  my $self = $class->SUPER::new( %params );
  eval{
      $self->{'PfamReg_acc'} = $acc;
      $self->{'PfamReg_id'} = $id;
      $self->{'PfamReg_model_from'} = $mod_st;
      $self->{'PfamReg_model_to'} = $mod_en;
      $self->{'PfamReg_bits_score'} = $bits;
      $self->{'PfamReg_evalue_score'} = $evalue;
      $self->{'PfamReg_domain_bits_score'} = $domain_bits;
      $self->{'PfamReg_domain_evalue_score'} = $domain_evalue;
      $self->{'PfamReg_sequence_bits_score'} = $sequence_bits;
      $self->{'PfamReg_sequence_evalue_score'} = $sequence_evalue;
      $self->{'PfamReg_mode'} = $mode;
      $self->{'PfamReg_in_full'} = $in_full;
      $self->{'PfamReg_is_significant'} = $is_significant;
      $self->{'pfamseq_id'} = $pfamseq_id;
      $self->{'pfamseq_acc'} = $pfamseq_acc;
      $self->{'pfamseq_desc'} = $pfamseq_desc;
      $self->{'tree_order'} = $tree_order;
      $self->{'auto_pfamA'} = $auto_pfamA;
      $self->{'type'} = $type;
      $self->{'region'} = $region;
      $self->{'model_length'} = $mod_len;

  };

  #$self->accession( $acc );
  #$self->id( $id );
  #$self->model_from( $mod_st );
  #$self->model_to( $mod_en );
  #$self->bits_score( $bits );
  #$self->evalue_score( $evalue );
  #$self->domain_bits_score( $domain_bits );
  #$self->domain_evalue_score( $domain_evalue );
  #$self->sequence_bits_score( $sequence_bits );
  #$self->sequence_evalue_score( $sequence_evalue );
  #$self->mode($mode);
  #$self->is_significant($is_significant);
  #$self->in_full($in_full);
  #$self->pfamseq_id($pfamseq_id);
  #$self->tree_order($tree_order);
  #$self->auto_pfamA($auto_pfamA);
  #$self->type($type);
  #$self->region($region);
  #$self->model_length($mod_len);
  
  # could argue that these shouldn't be here, but they make
  # PfamRCS::make_output_from_rdb a whole lot quicker
  #$self->pfamseq_acc($pfamseq_acc);
  #$self->pfamseq_desc($pfamseq_desc);

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
       $self->{'PfamReg_model_from'} = $value;
   }
   return $self->{'PfamReg_model_from'};
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
       $self->{'PfamReg_model_to'} = $value;
   }
   return $self->{'PfamReg_model_to'};
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
       $self->{'PfamReg_bits_score'} = $value;
   }
   return $self->{'PfamReg_bits_score'};
}



=head2 evalue_score

 Title   : evalue_score
 Usage   : 
    $reg->evalue_score(); # or ...
    $reg->evalue_score(15 );
 Function: For setting and getting score of this PfamRegion with respect to the model

=cut

sub evalue_score {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'PfamReg_evalue_score'} = $value;
   }
   return $self->{'PfamReg_evalue_score'};
}


sub domain_bits_score {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'PfamReg_domain_bits_score'} = $value;
   }
   return $self->{'PfamReg_domain_bits_score'};
}

sub domain_evalue_score {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'PfamReg_domain_evalue_score'} = $value;
   }
   return $self->{'PfamReg_domain_evalue_score'};
}

sub sequence_bits_score {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'PfamReg_sequence_bits_score'} = $value;
   }
   return $self->{'PfamReg_sequence_bits_score'};
}

sub sequence_evalue_score {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'PfamReg_sequence_evalue_score'} = $value;
   }
   return $self->{'PfamReg_sequence_evalue_score'};
}

sub mode {
   my ($self, $value) = @_;
   if (defined $value) {
       $self->{'PfamReg_mode'} = $value;
   }
   return $self->{'PfamReg_mode'};
}

sub in_full  {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'PfamReg_in_full'} = $value;
   }
   return $self->{'PfamReg_in_full'};
}


sub is_significant {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'PfamReg_is_significant'} = $value;
   }
   return $self->{'PfamReg_is_significant'};
}

sub pfamseq_id {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'pfamseq_id'} = $value;
   }
   return $self->{'pfamseq_id'};
}

sub pfamseq_acc {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'pfamseq_acc'} = $value;
   }
   return $self->{'pfamseq_acc'};
}


sub pfamseq_desc {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'pfamseq_desc'} = $value;
   }
   return $self->{'pfamseq_desc'};
}


sub tree_order {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'tree_order'} = $value;
   }
   return $self->{'tree_order'};
}

sub auto_pfamA {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'auto_pfamA'} = $value;
   }
   return $self->{'auto_pfamA'};
}

sub length {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'length'} = $value;
   }
   return $self->{'length'};
}

sub type {
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'type'} = $value;
   }
   return $self->{'type'};
}

sub region {
  my ($self, $value) = @_;
  if (defined $value) {
    $self->{'region'} = $value;
  }
  return $self->{'region'};
}

sub model_length {
   my ($self, $value) = @_;
   if (defined $value) {
       $self->{'model_length'} = $value;
   }
   return $self->{'model_length'};
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
