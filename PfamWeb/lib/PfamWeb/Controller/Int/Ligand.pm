# Ligand.pm
# rdf 20060818 WTSI
#
# $Id: Ligand.pm,v 1.5 2007-03-15 14:06:11 jt6 Exp $
# Controller to build the main Ligand page.
#

=head1 NAME

PfamWeb::Controller::Int::Ligand -  controller to build the main iPfam ligand
page.

=cut

package PfamWeb::Controller::Int::Ligand;

=head1 DESCRIPTION

Controller to build the main iPfam ligand page.

$Id: Ligand.pm,v 1.5 2007-03-15 14:06:11 jt6 Exp $

=cut

use strict;
use warnings;

use base "PfamWeb::Controller::Section";

# define the name of the section...
__PACKAGE__->config( SECTION => "ligand" );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 generateLigandIntSum : Path

Retrieves a Ligand based on the supplied three-letter-code.

=cut

sub generateLigandIntSum : Path {
  my( $this, $c ) = @_;

  if( defined $c->req->param("code") ) {
    $c->req->param("code") =~ m/^(\w{3})$/i;
    $c->stash->{ligand} = $c->model("PfamDB::Ligands")
	  ->find( { three_letter_code => $1 } );

    $c->log->debug("Int::Ligand::generateLigandIntSum: Got ligand data for:".$c->stash->{ligand}->name.":");
  }

}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

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

1;
