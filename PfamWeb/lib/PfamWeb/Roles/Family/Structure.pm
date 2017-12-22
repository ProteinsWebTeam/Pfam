# Structure.pm
# jt6 20120920 WTSI
#
# $Id$

=head1 NAME

PfamWeb::Roles::Family::Structure - role to add structure-related methods to
family controller

=cut

package PfamWeb::Roles::Family::Structure;

=head1 DESCRIPTION

This role adds structure-related methods to the L<Family> controller.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

#-------------------------------------------------------------------------------

=head1 STRUCTURE ACTIONS

=head2 structures : Chained

Retrieves the list of PDB entries for this family. If a PDB ID is specified,
the method also retrieves the row of the "pdb" table for that entry.

=cut

sub structures : Chained( 'family' )
                 PathPart( 'structures' )
                 Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  # see if we were handed a valid PDB ID and, if so, just stash it
  if ( defined $c->req->param('pdbId') and
       $c->req->param('pdbId') =~ /^(\d\w{3})$/ ) {

    $c->log->debug( "Family::structures: got PDB ID: |$1|" )
      if $c->debug;

    $c->stash->{pdb_id} = $1;
  }

  # retrieve the PDB entries for this family
  my @regions;
  if ( defined $c->stash->{pfam} and $c->stash->{pfam}->pfama_acc ) {
    $c->log->debug( 'Family::structures: got an pfama_acc: '
                    . $c->stash->{pfam}->pfama_acc ) if $c->debug;
    @regions = $c->model('PfamDB::PdbPfamaReg')
                 ->search( { 'me.pfama_acc' => $c->stash->{pfam}->pfama_acc },
                           { prefetch => [ qw( pdb_id pdb_image pfama_acc ) ] } );
    $c->log->debug( 'Family::structures: got '
                    . scalar @regions . ' regions' ) if $c->debug;
  }

  # don't render the template unless we need to
  unless ( scalar @regions ) {
    $c->log->debug( 'Family::structures: no structure image; not rendering template' )
      if $c->debug;
    $c->res->status( 204 );
    return;
  }

  my $pdb_unique = {};
  my $colours = {};
  foreach my $region ( @regions ) {
    my $id = $region->pdb_id->pdb_id;
    $pdb_unique->{$id} = $region;
    $colours->{$id}->{$region->hex_colour} = $region->pfama_acc->pfama_id
      if $region->hex_colour;
  }

  $c->stash->{pdb_unique} = $pdb_unique;
  $c->stash->{colours}    = $colours;

  # my %pdb_unique = map{ $_->pdb_id->pdb_id => $_ } @regions;
  # $c->stash->{pdb_unique} = \%pdb_unique;

  # set up the view and rely on "end" from the parent class to render it
  $c->stash->{template} = 'components/blocks/family/familyStructures.tt';

  # cache the template output for one week
  $c->cache_page( 604800 );
}

#---------------------------------------

=head2 old_structures : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_structures : Path( '/family/structures' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_structures: redirecting to "structures"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, 'structures', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 mapping : Chained

Renders a table showing the mapping between Pfam family, UniProt region and
PDB residues.

=cut

sub mapping : Chained( 'family' )
              PathPart( 'mapping' )
              Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  $c->log->debug( 'Family::FamilyActions::mapping: acc: |'
                  . $c->stash->{acc}  . '|' .  $c->stash->{entryType}. '|' )
    if $c->debug;

  my @mapping = $c->model('PfamDB::PdbPfamaReg')
                  ->search( { pfama_acc => $c->stash->{pfam}->pfama_acc },
                            { join      => [ qw( pdb_id uniprot_acc ) ],
                              columns   => [ qw( uniprot_acc.uniprot_id
                                                 seq_start
                                                 seq_end
                                                 pdb_id.pdb_id
                                                 chain
                                                 pdb_res_start
                                                 pdb_res_end ) ] } );

  # my @mapping;
  $c->log->debug( '!!!' . scalar @mapping . '!!!' ) if $c->debug;
  # sleep 5;
  $c->stash->{pfamMaps} = \@mapping;
  $c->log->debug( 'Family::FamilyActions::mapping: found |' . scalar @mapping . '| rows' )
    if $c->debug;

  unless ( scalar @mapping ) {
    $c->log->debug( 'Family::FamilyActions::mapping: no rows; returning 204' )
      if $c->debug;

    $c->res->status( 204 );

    return;
  }

  if ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Family::FamilyActions::mapping: emitting XML' ) if $c->debug;
    $c->stash->{template} = 'rest/family/structures_xml.tt';
  }
  else {
    $c->log->debug( 'Family::FamilyActions::mapping: emitting HTML' ) if $c->debug;
    $c->stash->{template} = 'components/blocks/family/structureTab.tt';
  }

  # cache the template output for one week
  $c->cache_page( 604800 );
}

#---------------------------------------

=head2 old_mapping : Path

Deprecated. Stub to redirect to chained action.

=cut

sub old_mapping : Path( '/family/structures/mapping' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::FamilyActions::old_mapping: redirecting to "mapping"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->secure_uri_for( '/family', $c->stash->{param_entry}, 'mapping', $c->req->params ) );
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
