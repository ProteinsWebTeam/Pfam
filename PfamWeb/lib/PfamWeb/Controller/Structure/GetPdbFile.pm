
# GetPdbFile.pm
# jt6 20060731 WTSI
#
# $Id: GetPdbFile.pm,v 1.4 2007-03-15 14:06:15 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure::GetPdbFile - serve a PDB file

=cut

package PfamWeb::Controller::Structure::GetPdbFile;

=head1 DESCRIPTION

Retrieves a specified PDB file from the model and serves it up with
the appropriate MIME type (chemical/x-pdb). Relies on
L<Structure::begin|PfamWeb::Controller::Structure/"begin : Private"> to
extract the PDB ID from the URL.

Generates a B<flat file>, MIME type C<chemical/x-pdb>.

$Id: GetPdbFile.pm,v 1.4 2007-03-15 14:06:15 jt6 Exp $

=cut

use strict;
use warnings;

use base "PfamWeb::Controller::Structure";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 default : Path

Pick up http://localhost:3000/structure/getpdbfile?id=1abc and similar
URLs. Retrieves the file from the model. Currently the model uses the
WTSI pfetch server only.

=cut

sub default : Path {
  my( $this, $c, $pdbId ) = @_;

  return unless defined $c->stash->{pdbId};

  $c->stash->{pdbFile} = 
    $c->model( "Pfetch" )->retrieve( { "--pdb" => $c->stash->{pdbId} } );

}

#-------------------------------------------------------------------------------

=head2 end : Private

Push the file to the response

=cut

sub end : Private {
  my( $this, $c ) = @_;

  # don't try to render a page unless there's a Pdb object in the stash
  return 0 unless defined $c->stash->{pdbFile};

  $c->res->content_type( "chemical/x-pdb" );
  foreach ( @{$c->stash->{pdbFile}} ) {
	$c->res->write( $_ );
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
