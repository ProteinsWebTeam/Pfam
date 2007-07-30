
# GetPdbFile.pm
# jt6 20060731 WTSI
#
# $Id: GetPdbFile.pm,v 1.6 2007-07-30 12:39:12 jt6 Exp $

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

$Id: GetPdbFile.pm,v 1.6 2007-07-30 12:39:12 jt6 Exp $

=cut

use strict;
use warnings;

use LWP::Simple;

use base 'PfamWeb::Controller::Structure';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 getPdbFile : Path

Returns the contents of the PDB file for the specified PDB entry. The PDB
entry can be specified in various ways. See L<begin> for details.

=cut

sub getPdbFile : Path {
  my( $this, $c, $pdbId ) = @_;

  return unless defined $c->stash->{pdbId};

  my $cacheKey = 'pdb' . $c->stash->{pdbId};

  my $pdbFile;
  if( $pdbFile = $c->cache->get( $cacheKey ) ) {
    $c->log->debug( 'Structure::GetPdbFile::getPdbFile: retrieved |'
                    . $c->stash->{pdbId} . '| from cache' ); 
  } else {
    $c->log->debug( 'Structure::GetPdbFile::getPdbFile: retrieving |'
                    . $c->stash->{pdbId} . '| from remote site' ); 
  
    my $url = $this->{pdbFileUrl} . 'pdb' . $c->stash->{pdbId} . '.ent';
    $c->log->debug( "Structure::GetPdbFile::getPdbFile: looking for file at: |$url|" );
  
    $pdbFile = get( $url );
    $c->cache->set( $cacheKey, $pdbFile );
  }
  
  return unless defined $pdbFile;

  my $filename = 'pdb' . $c->stash->{pdbId} . '.ent';
  $c->res->headers->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->content_type( 'chemical/x-pdb' );
  $c->res->body( $pdbFile );
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