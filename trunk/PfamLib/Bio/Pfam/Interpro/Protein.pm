
#
# BioPerl module for Bio::Interpro::Protein
#
# Cared for by David Studholme, Pfam <pfam@sanger.ac.uk>
#
# Copyright David Studholme, Pfam
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code




=head1 DESCRIPTION

This module defines a class of Protein objects
The properties of a Protein are its id, and length ;  and 
it also may contain one or more Match objects

=head1 CONTACT

This module was developed originally by David Studholme <ds2@sanger.ac.uk>
The most stable contact email is probably pfam@sanger.ac.uk

=cut



package Bio::Pfam::Interpro::Protein ;

use strict;

sub new {
  my( $class,@args ) = @_;
  my $self  = {} ;
  bless $self, $class ;
  $self->{IP_PROTEIN_MATCHES} = [] ;
  return $self; 
}


sub len {
  my ( $self, $specified ) = @_ ;
  if ( $specified ) {
    $self->{IP_PROTEIN_LEN} = $specified
  }
  return  $self->{IP_PROTEIN_LEN} 
}


sub id {
  my ( $self, $specified ) = @_ ;
  if ( $specified ) {
    $self->{IP_PROTEIN_ID} = $specified 
  }
  return  $self->{IP_PROTEIN_ID} 
}

sub acc {
  my ( $self, $specified ) = @_ ;
  if ( $specified ) {
    $self->{IP_PROTEIN_ACC} = $specified 
  }
  return  $self->{IP_PROTEIN_ACC} 
}


sub name {
  my ( $self, $specified ) = @_ ;
  if ( $specified ) {
    $self->{IP_PROTEIN_NAME} = $specified
  }
  return  $self->{IP_PROTEIN_NAME} 
}

sub matches {  
  my ( $self, @specified ) = @_ ;
  my $matches_ref = $self->{IP_PROTEIN_MATCHES} ;  
  if ( @specified ) {
    push @$matches_ref, @specified ;
  }

  return @$matches_ref ;
}


return 1 ;  # end of module

