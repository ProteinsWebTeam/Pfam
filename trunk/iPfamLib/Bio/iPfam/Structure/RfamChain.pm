
# RfamChain.pm
# rdf/jt6 20080125 WTSI
#
# $Id: RfamChain.pm,v 1.1 2008-03-03 13:16:14 rdf Exp $

=head1 NAME

Chain - store chain details

=cut

package Bio::iPfam::Structure::RfamChain;

=head1 SYNOPSIS

=head1 DESCRIPTION

The region object contains the information on sub part of a chain found within the structure.  

$Id: RfamChain.pm,v 1.1 2008-03-03 13:16:14 rdf Exp $

=cut

use strict;
use warnings;

use Carp;
use Text::Wrap;

use Data::Dump qw( dump );

use Bio::iPfam::Structure::Region;


use base qw( Bio::iPfam::Structure::Chain );

# getters and setters for simple object properties
__PACKAGE__->mk_ro_accessors( qw( regions ) );

# logging ! Log early, log often
use Log::Log4perl qw( get_logger );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 add_monomer

Adds a L<Monomer|Bio::iPfam::Structure::Monomer> to this chain. Returns a 
reference to the added monomer.

  $chain->add_monomer( $monomer );

=cut
  
sub add_region {
  my ( $self, $region ) = @_;

  unless ( defined $region and 
           $region->isa( 'Bio::iPfam::Structure::Region' ) ) {
    carp q(warning: can't add region; not a Region object);
    return;
  }

  push @{ $self->{regions} }, $region;
  


  if ( defined $region->acc and defined $region->start and defined $region->end) {  
    # build a "fingerprint" for the monomer and store it in a hash using that
    # as the hash key
    my $fp = $region->acc."/".$region->start."-".$region->end;
    $self->{region_hash}->{$fp} = $region;
  }else {
    carp q(warning: chian does not have a unique ID so will not be able to retrieve the domain by unique ID);
  }
  
  return $region;
}

#-------------------------------------------------------------------------------

=head2 get_region

Returns a L<Monomer|Bio::iPfam::Structure::Monomer> based on the identifiers
provided. The argument to this method should be either a hashref, an arrayref
or a scalar. If the argument is a hashref, the hash should contain three 
keys:

=over 4

=item chainID: chain identifier (single character, defaults to ' ')

=item resSeq:  monomer number

=item iCode:   insertion code (non-mandatory)

=back

If the argument is an arrayref, the array should contain the same three values,
in the same order.

If the value is a scalar, it should contain a monomer "fingerprint" constructed
by concatenating these three values in the same order.

Returns the L<Monomer> or undef if that monomer is not found in this chain.

=cut
  
sub get_region {
  my ( $self, $identifier ) = @_;
  
  unless ( defined $identifier ) {
    carp q(warning: can't get region without region id);
    return;
  }

  return $self->{region_hash}->{$identifier};
}

#-------------------------------------------------------------------------------

=head2 delete_monomer

Deletes a L<Monomer|Bio::iPfam::Structure::Monomer> to this chain. Returns a 
reference to the deleted monomer or undef if the monomer that was handed in
was not part of this chain.

  $chain->delete_monomer( $monomer );

=cut
  
sub delete_region {
  my ( $self, $region ) = @_;

  unless ( defined $region and 
           $region->isa( 'Bio::iPfam::Structure::Region' ) ) {
    carp q(warning: can't delete region; not a Region object);
    return;
  }

  for ( my $i = 0; $i < scalar @{ $self->{regions} }; $i++ ) {
    return splice( @{ $self->{regions} }, $i, 1 )
      if $region eq $self->{regions}->[$i];
  }
  
  return undef;
}

#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------

=head1 COPYRIGHT

Copyright (c) 2008: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;

