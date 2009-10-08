# Clan.pm
#
# Author:        rdf
# Maintainer:    $Id: Clan.pm,v 1.3 2009-10-08 12:27:28 jt6 Exp $
# Version:       $Revision: 1.3 $
# Created:       Apr 24, 2009
# Last Modified: $Date: 2009-10-08 12:27:28 $

=head1 NAME

Template - a short description of the class

=cut

package Bio::Pfam::Clan::Clan;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: Clan.pm,v 1.3 2009-10-08 12:27:28 jt6 Exp $

=head1 COPYRIGHT

File: Clan.pm

Copyright (c) 2007: Genome Research Ltd.

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


use strict;
use warnings;

use Moose;
use Moose::Util::TypeConstraints;

#use Bio::Pfam::ClanIO;
use Bio::Pfam::Clan::DESC;

subtype 'DESCClan'
  => as Object
  => where { $_->isa('Bio::Pfam::Clan::DESC') }
  => message { "Couldn't create a Bio::Pfam::Clan::DESC object" };



coerce 'DESCClan'
  => from 'Str'
    => via {
         my $clanDescIO = Bio::Pfam::ClanIO->new;
         my $desc;
         eval { 
           $desc = $clanDescIO->parseCLANDESC( $_ );
         };
         return $@ ? undef : $desc;
         
       }
  => from 'GlobRef'
    => via {
        my $clanDescIO = Bio::Pfam::ClanIO->new;
         my $desc;
         eval { 
           $desc = $clanDescIO->parseCLANDESC( $_ );
         };
         return $@ ? undef : $desc;
         
       }
  => from 'HashRef'
    => via {
      my $desc;
      eval{
        $desc = Bio::Pfam::Clan::DESC->new( $_ );
      };
      return $@ ? undef : $desc; 
    };   
    

#----------------------------------------

has 'DESC' => (
  is        => 'ro',
  isa       => 'DESCClan',
  required  => 1,
  coerce    => 1
);

has 'source' => (
  is    => 'ro',
  isa   => enum ([ qw(database file svn) ]),
);

has 'rdb' => (
  is => 'rw',
  isa => 'HashRef',
);

#----------------------------------------

sub familyIsMember {
  my($self,  $family) = @_;
  
  my $seen = 0;

  if ($self->DESC->MEMB){
    foreach my $m (@{ $self->DESC->MEMB }){
      if($m eq $family){
        $seen = 1;
        last;
      }
    }  
  }
  
  return($seen);
}




1;
