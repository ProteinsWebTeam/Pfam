package RfamLive::ResultSet::Clan;

use strict;
use warnings;
use Carp;
use Data::Printer;
use DateTime;
use DateTime::Format::MySQL;

use base 'DBIx::Class::ResultSet';

sub id2acc {
  my ( $self, $id ) = @_;

  my $clan = $self->find( { id => $id } );

  return ( defined($clan) ? $clan->clan_acc() : undef );
}

sub updateClanFromObj {
  my ( $self, $clanObj) = @_;
  
  if ( !$clanObj or !$clanObj->isa('Bio::Rfam::Clan') ) {
    croak(
'Either the Bio::Rfam::Clan object was undefined or not an object of that type.'
    );
  }

  my $clan = $self->find( { clan_acc => $clanObj->DESC->AC } );

  unless ($clan) {
    croak("Failed to find a database entry in the clan table for "
        . $clanObj->DESC->AC
        . ". This is really bad!\n" );
  }

  #We have found a family, so now set all the essential attributes and
  #zero all calculated data fields.

  $clan->id( $clanObj->DESC->ID );
  $clan->description( $clanObj->DESC->DE );
  $clan->author( $clanObj->DESC->AU );
  $clan->comment( $clanObj->DESC->CC ) if(defined($clanObj->DESC->CC));
  $clan->previous_id( $clanObj->DESC->PI ) if(defined($clanObj->DESC->PI) );
 
  my $dt = DateTime->now( time_zone  => 'Europe/London');
  $clan->updated( DateTime::Format::MySQL->format_datetime($dt));
  
  $clan->update;
}

sub createClanFromObj {
  my ( $self, $clanObj ) = @_;
  if ( !$clanObj or !$clanObj->isa('Bio::Rfam::Clan') ) {
    croak(
'Either the Bio::Rfam::Clan object was undefined or not an object of that type.'
    );
  }
  my $fam = $self->find( { clan_acc => $clanObj->DESC->AC } );
  if ($fam) {
    croak("Found a database entry in the family table for "
        . $clanObj->DESC->AC
        . " when there should not be one. This is really bad!\n" );
  }
  my $dt = DateTime->now( time_zone  => 'Europe/London');
  $self->create(
    {
      #No previous ID as it is new.
      clan_acc    =>  $clanObj->DESC->AC,
      id          =>  $clanObj->DESC->ID,
      description =>  $clanObj->DESC->DE, 
      author      => $clanObj->DESC->AU,
      comment     => defined($clanObj->DESC->CC) ? $clanObj->DESC->CC : ''.
      updated     => DateTime::Format::MySQL->format_datetime($dt),
      created     => \'NOW()'
    }
  );
}


1;
