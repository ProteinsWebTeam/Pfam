
# Structure.pm
# jt6 20120809 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Controller::Structure - controller to handle PDB structure-related calls

=cut

package RfamWeb::Controller::Structure;

=head1 DESCRIPTION

This is the controller for everything related to PDB structures.

$Id$

=cut

use Moose;
use namespace::autoclean;

use LWP::Simple;

# set the name of the section
__PACKAGE__->config( SECTION => 'structure' );

BEGIN {
  extends 'Catalyst::Controller';
}

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 structure: Chained('/') PathPart('structure') CaptureArgs(1)

Start of a chain. Captures the PDB ID from the URL.

=cut

sub structure : Chained( '/' )
                PathPart( 'structure' )
                CaptureArgs( 1 ) {
  my ( $this, $c, $pdb_id ) = @_;

  return unless ( defined $pdb_id and
                  $pdb_id =~ m/(\d\w{3})/ );

  $c->log->debug( "Family::structure: captured |$1| from URL" )
    if $c->debug;  

  $c->stash->{pdb_id} = $1;
}

#-------------------------------------------------------------------------------

=head2 pdb : Chained('structure') PathPart('') Args(0)

Returns the PDB file for the given PDB ID.

=cut

sub pdb : Chained( 'structure' )
          PathPart( '' )
          Args( 0 ) {
  my ( $this, $c ) = @_;

  my $pdb_file;
  my $cache_key = 'pdb' . $c->stash->{pdb_id};

  if ( $pdb_file = $c->cache->get( $cache_key ) ) { 
    $c->log->debug( 'Structure::pdb: retrieved |' . $c->stash->{pdb_id} . '| from cache' )
      if $c->debug;  
  }
  else {
    $c->log->debug( 'Structure::pdb: retrieving |' . $c->stash->{pdb_id} . '| from remote site' ) 
     if $c->debug;

    # build the full URL
    my $url = $this->{pdbFileUrl} . 'pdb' . $c->stash->{pdb_id} . '.ent';
    $c->log->debug( "Structure::pdb: looking for file at: |$url|" )
      if $c->debug;

    # retrieve the file itself
    $pdb_file = get( $url );
  
    # cache it
    if ( defined $pdb_file ) {
      $c->log->debug( "Structure::pdb: successfully downloaded PDB file for |$url|" )
        if $c->debug;
      $c->cache->set( $cache_key, $pdb_file ) unless $ENV{NO_CACHE};
    }

  }
  
  return unless defined $pdb_file;

  $c->log->debug( 'Structure::pdb: successfully retrieved ' . $c->stash->{pdb_id} )
    if $c->debug;

  my $filename = 'pdb' . $c->stash->{pdb_id} . '.ent';
  $c->res->headers->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->content_type( 'chemical/x-pdb' );
  $c->res->body( $pdb_file );
}

#-------------------------------------------------------------------------------

# can't get the jmol applet to read a PDB file successfully from the server,
# so rather than debug the connection problem between the two, we're just 
# going to ditch jmol for now.
# jt6 20100809 WTSI
#
# sub jmol : Chained( 'structure' )
#            PathPart( 'jmol' )
#            Args( 0 ) {
#   my ( $this, $c ) = @_;
# 
#   $c->log->debug( 'Structure::jmol: showing JMol for ' . $c->stash->{pdb_id} )
#     if $c->debug;
# 
#   my @rs = $c->model('RfamDB::PdbRfamReg')
#              ->search( { pdb_id => $c->stash->{pdb_id} },
#                        { prefetch => [ qw( auto_rfam ) ],
#                          order_by => 'chain ASC' } );
#   if ( @rs ) {
#     $c->log->debug( 'Structure::jmol: found ' . scalar @rs . ' mappings' )
#       if $c->debug;
#     $c->stash->{mapping} = \@rs;
#   }
# 
#   $c->stash->{template} = 'components/tools/jmol.tt';
# }

#-------------------------------------------------------------------------------

=head2 av : Chained('structure') PathPart('av') Args(0)

Returns the HTML for showing the AstexViewer applet and associated controls.

=cut

sub av : Chained( 'structure' )
         PathPart( 'av' )
         Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Structure::av: showing av for ' . $c->stash->{pdb_id} )
    if $c->debug;

  my @rs = $c->model('RfamDB::PdbFullRegion')
             ->search( { pdb_id => $c->stash->{pdb_id} },
                       { prefetch => [ qw( rfam_acc ) ],
                         order_by => 'chain ASC' } );
  if ( @rs ) {
    $c->log->debug( 'Structure::av: found ' . scalar @rs . ' mappings' )
      if $c->debug;
    $c->stash->{mapping} = \@rs;
  }

  $c->stash->{template} = 'components/tools/av.tt';
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Sarah Burge, C<sb30@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk), 
         Jennifer Daub (jd7@sanger.ac.uk)

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

__PACKAGE__->meta->make_immutable;

1;

