package RfamLive::ResultSet::MotifDatabaseLink;

use strict;
use warnings;
use Carp;

use base 'DBIx::Class::ResultSet';

sub find_or_createFromMotifObj {
  my ($self, $motifObj) = @_;
  
  if(!$motifObj or !$motifObj->isa('Bio::Rfam::Motif')){
    croak('Either the Bio::Rfam::Motif object was undefined or not an object of that type.');
  }
  if(defined($motifObj->DESC->DBREFS)){
    foreach my $db (@{$motifObj->DESC->DBREFS}){
      my $link = $self->find_or_create( { motif_acc     => $motifObj->DESC->AC,
                                          db_id         => $db->{db_id},
                                          comment       => defined($db->{comment}) ? $db->{comment} : '',
                                          db_link       => $db->{db_link},
                                          other_params  => defined($db->{other_params}) ? $db->{other_params} : ''
      } );
    }
  }
}

1;
