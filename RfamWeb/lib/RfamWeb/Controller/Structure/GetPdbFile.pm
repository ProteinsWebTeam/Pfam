
# GetPdbFile.pm
# jt6 20060731 WTSI
#
# $Id: GetPdbFile.pm,v 1.1 2008-10-23 10:56:26 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Structure::GetPdbFile - serve a PDB file

=cut

package RfamWeb::Controller::Structure::GetPdbFile;

=head1 DESCRIPTION

Retrieves a specified PDB file from the model and serves it up with
the appropriate MIME type (chemical/x-pdb). Relies on
L<Structure::begin|PfamWeb::Controller::Structure/"begin : Private"> to
extract the PDB ID from the URL.

Generates a B<flat file>, MIME type C<chemical/x-pdb>.

$Id: GetPdbFile.pm,v 1.1 2008-10-23 10:56:26 jt6 Exp $

=cut

use strict;
use warnings;

use LWP::Simple;

use base 'RfamWeb::Controller::Structure';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 getPdbFile : Path

Returns the contents of the PDB file for the specified PDB entry.

=cut

sub getPdbFile : Path {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pdbId};

  my $cacheKey = 'pdb' . $c->stash->{pdbId};

  if( $c->stash->{pdbFile} = $c->cache->get( $cacheKey ) ) {
    $c->log->debug( 'Structure::GetPdbFile::getPdbFile: retrieved |'
                    . $c->stash->{pdbId} . '| from cache' ); 
  } else {
    $c->log->debug( 'Structure::GetPdbFile::getPdbFile: retrieving |'
                    . $c->stash->{pdbId} . '| from remote site' ); 
  
    # right now we're just forwarding to the only method for retrieving a
    # PDB file, which gets it from a URL using LWP::Simple. Ideally we should
    # be checking the configuration to see if there's a local copy of the 
    # PDB and retrieving a file directly from there, if possible.
    # TODO check for a local PDB mirror
    $c->forward( 'getPdbFileFromUrl' );
  
    # cache it
    $c->cache->set( $cacheKey, $c->stash->{pdbFile} ) unless $ENV{NO_CACHE};

  }
  
  return unless defined $c->stash->{pdbFile};

  my $filename = 'pdb' . $c->stash->{pdbId} . '.ent';
  $c->res->headers->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->content_type( 'chemical/x-pdb' );
  $c->res->body( $c->stash->{pdbFile} );
}

#-------------------------------------------------------------------------------

=head2 getPdbFileFromUrl : Path

Retrieves the specified PDB file from a URL.

=cut

sub getPdbFileFromUrl : Private {
  my( $this, $c ) = @_;

  # build the full URL
  my $url = $this->{pdbFileUrl} . 'pdb' . $c->stash->{pdbId} . '.ent';
  $c->log->debug( "Structure::GetPdbFile::getPdbFileFromUrl: looking for file at: |$url|" );

  # retrieve the file itself
  my $pdbFile = get( $url );
  return unless defined $pdbFile;

  # and stash it
  $c->stash->{pdbFile} = $pdbFile;  
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;