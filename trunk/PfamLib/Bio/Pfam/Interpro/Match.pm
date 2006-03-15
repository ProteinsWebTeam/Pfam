
#
# BioPerl module for Bio::Interpro::Match
#
# Cared for by David Studholme, Pfam <pfam@sanger.ac.uk>
#
# Copyright David Studholme, Pfam
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code




=head1 DESCRIPTION



=head1 CONTACT

This module was developed originally by David Studholme <ds2@sanger.ac.uk>
The most stable contact email is probably pfam@sanger.ac.uk

=cut



package Bio::Pfam::Interpro::Match;


use strict;



sub new {
  my $self = {} ;
  bless $self ;
  return $self ;
}




sub id {
  my ( $self, $specified ) = @_ ;
  if ( $specified ) {
    $self->{IP_MATCH_ID} = $specified 
  }
  return  $self->{IP_MATCH_ID} 
}


sub name {
  my ( $self, $specified ) = @_ ;
  if ( $specified ) {
    $self->{IP_MATCH_NAME} = $specified;
  }
  return  $self->{IP_MATCH_NAME} 
}



sub start {
  my ( $self, $specified ) = @_ ;
  if ( $specified ) {
    $self->{IP_MATCH_START} = $specified
  }
  return  $self->{IP_MATCH_START} 
}


sub end {
  my ( $self, $specified ) = @_ ;
  if ( $specified ) {
    #print "Match->end: argument $specified\n" ;
    $self->{IP_MATCH_END} = $specified
  }
  #print ("Match->end: returning ", $self->{IP_MATCH_END} , "\n") ;
  return  $self->{IP_MATCH_END} 
}



sub status {
  my ( $self, $specified ) = @_ ;
  if ( $specified ) {
    $self->{IP_MATCH_STATUS} = $specified
  }
  return  $self->{IP_MATCH_STATUS} 
}


sub interpro_acc {
  my ( $self, $specified ) = @_ ;
  if ( $specified ) {
    $self->{IP_MATCH_INTERPRO} = $specified
  }
  return  $self->{IP_MATCH_INTERPRO} 
}

sub merops_fam {
  my ( $self, $specified ) = @_ ;
  if ( $specified ) {
    $self->{IP_MATCH_MEROPS_FAM} = $specified
  }
  return  $self->{IP_MATCH_MEROPS_FAM} 
}

sub merops_subfam {
  my ( $self, $specified ) = @_ ;
  if ( $specified ) {
    $self->{IP_MATCH_MEROPS_SUBFAM} = $specified
  }
  return  $self->{IP_MATCH_MEROPS_SUBFAM} 
}

sub pfam {
  my ( $self, $specified ) = @_ ;
  if ( $specified ) {
    $self->{IP_MATCH_PFAM} = $specified
  }
  return  $self->{IP_MATCH_PFAM} 
}


sub evidence {
  my ( $self, $specified ) = @_ ;
  if ( $specified ) {
    $self->{IP_MATCH_EVIDENCE} = $specified
  }
  return  $self->{IP_MATCH_EVIDENCE} 
}





return 1 ;  # end of module
