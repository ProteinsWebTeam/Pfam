#
# BioPerl module for Bio::Pfam::MeropsRegion
#
# Cared for by Rob Finn <rdf@sanger.ac.uk>
#
# Copyright Pfam
#
# You may distribute this module under the same terms as perl itself
# POD documentation - main docs before the code

=head1 NAME

Bio::Pfam::MeropsRegion - Representation of a Merops domain in Pfam

=head1 SYNOPSIS

    use Bio::Pfam::MeropsRegion;

    $MeropsRegion = new Bio::Pfam::MeropsRegion( '-MEROPS_ACCESSION' => $acc,
					   '-MEROPS_ID' => $id,
					   '-VERSION' => $seq_id,
                                           '-BITS' => bits,
                                           '-EVALUE' => $eval,
					   '-FROM' => $start,
					   '-TO' => $end);

=head1 DESCRIPTION

This object stores the details for a Merops domain. It is derived from the
AnnotatedRegion class. Information that must be given is the name and Merops 
accession number of the domain, the extent of the domain (in terms of start and end
indices), and an annotation for the domain.


=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut

# $Author: jt6 $

# Let the code begin...


package Bio::Pfam::MeropsRegion;
use vars qw($AUTOLOAD @ISA);
use strict;
use warnings;

use Bio::Pfam::AnnotatedRegion;
@ISA = qw(Bio::Pfam::AnnotatedRegion);


sub new {
  my( $class, %params ) = @_;
  
  my $acc = ($params{'-MEROPS_ACCESSION'}||$params{'-merops_accession'});
  my $id = ($params{'-MEROPS_ID'}||$params{'-merops_id'});
  my $pfam = ($params{'-PFAM'}||$params{'-pfam'});
  my $cjag = ($params{'-CJAG'}||$params{'-cjag'});
  my $njag = ($params{'-NJAG'}||$params{'-njag'});
  my $bits = ($params{'-BITS'}||$params{'-bits'});
  my $evalue = ($params{'-EVALUE'}||$params{'-evalue'});
  my $type = ($params{'-TYPE'}||$params{'-type'});
  my $version = ($params{'-VERSION'}||$params{'-version'});
  my $self = $class->SUPER::new( %params );
  
  $self->accession( $acc );
  $self->id( $id );
  $self->pfam($pfam);
  $self->cjag($cjag);
  $self->njag($njag);
  $self->version($version);
  $self->bits_score( $bits );
  $self->evalue_score( $evalue );
  $self->type($type);

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
       $self->{'acc'} = $value;
   }
   return $self->{'acc'};
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
       $self->{'id'} = $value;
   }
   return $self->{'id'};
}

=head2 bits_score

 Title   : bits_score
 Usage   : 
    $reg->bits_score(); # or ...
    $reg->bits_score(15 );
 Function: For setting and getting bits score of this PfamRegion with respect to the model

=cut

=head2 pfam

 Title   : pfam
 Usage   : 
    $reg->pfam(); # or ...
    $reg->pfam( "helloSir" );
 Function: For setting and getting the PFAM field in the object

=cut

sub pfam{
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'pfam'} = $value;
   }
   return $self->{'pfam'};
}

=head2 cjag

 Title   : cjag
 Usage   : 
    $reg->cjag(); # or ...
    $reg->cjag( "helloSir" );
 Function: For setting and getting the CJAG field in the object

=cut

sub cjag{
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'cjag'} = $value;
   }
   return $self->{'cjag'};
}

=head2 njag

 Title   : njag
 Usage   : 
    $reg->njag(); # or ...
    $reg->njag( "helloSir" );
 Function: For setting and getting the NJAG field in the object

=cut

sub njag{
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'njag'} = $value;
   }
   return $self->{'njag'};
}

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

sub type {
  my ($self, $value) = @_;
  if (defined $value) {
    $self->{'type'} = $value;
  }
  return $self->{'type'};
}

sub version {
  my ($self, $value) = @_;
  if (defined $value) {
    $self->{'version'} = $value;
  }
  return $self->{'version'};
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
