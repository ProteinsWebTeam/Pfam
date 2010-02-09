
# Ligand.pm
# rdf/jt6 20080129 WTSI
#
# $Id: Ligand.pm,v 1.2 2008-01-30 15:40:50 jt6 Exp $

=head1 NAME

Ligand - store ligand details

=cut

package Bio::iPfam::Structure::Ligand;

=head1 SYNOPSIS

  use Bio::iPfam::Structure::Residue;

  $residue = Bio::iPfam::Structure::Residue->new;
  $residue = Bio::iPfam::Structure::Residue->new( $atom_record );

  use Bio::iPfam::Structure::Atom;
  $atom = Bio::iPfam::Structure::Atom->new( $atom_record );
  $residue->add_atom( $atom );

  $deleted_atom = $residue->delete_atom( $atom );

  $atom_count = $residue->atom_count;

  $residue->write; # print ATOM record to STDOUT
  $residue->write( \*FILEHANDLE );

  # residue property getters and setters
  $residue->resName( 'ALA' );
  $residue_name = $residue->resName;

  $residue->chainID( 'A' );
  $chain_id = $residue->chainID;

  $residue->resSeq( 123 );
  $residue_number = $residue->resSeq;

  $residue->iCode( 'A' );
  $insertion_code = $residue->iCode;

  $residue->secondary( 'E' );
  $secondary_struction = $residue->secondary;

  $residue->modified( 1 );
  $is_modified = $residue->modified;

  $primary_atom = $residue->primary;

=head1 DESCRIPTION

An object to store residue information. Essentially a bag of 
L<Atoms|Bio::iPfam::Structure::Atom>.

$Id: Ligand.pm,v 1.2 2008-01-30 15:40:50 jt6 Exp $

=cut

use strict;
use warnings;

use Carp;

use Bio::iPfam::Structure::Atom;

use base qw( Bio::iPfam::Structure::Residue );

# getters and setters for simple object properties
__PACKAGE__->mk_accessors( qw( 
                               hetID
                               numHetAtoms
                               description
                               formula
                             ) );

# logging ! Just gotta do it.
use Log::Log4perl qw( get_logger );

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
