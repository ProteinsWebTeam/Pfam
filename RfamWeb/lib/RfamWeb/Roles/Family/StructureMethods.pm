
# StructureMethods.pm
# jt6 20120514 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Roles::Family::StructureMethods - role to add protein structure-related
methods to the family page controller

=cut

package RfamWeb::Roles::Family::StructureMethods;

=head1 DESCRIPTION

This is a role to add protein structure-related methods to the Family controller.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

#-------------------------------------------------------------------------------

=head2 structures : Chained('family') PathPart('structures') Args(0)

Renders a table showing the mapping between Rfam family, UniProt region and
PDB residues.

=cut

sub structures : Chained( 'family' )
                 PathPart( 'structures' )
                 Args( 0 )
                 ActionClass( 'REST' ) {}

sub structures_GET : Private {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::structures: showing structures that map to ' . $c->stash->{acc} )
    if $c->debug;

  my @mapping = $c->stash->{db}->resultset('PdbRfamReg')
                  ->search( { auto_rfam => $c->stash->{rfam}->auto_rfam },
                            {} );

  unless ( scalar @mapping ) {
    $c->log->debug( 'Family::structures: no rows found; returning 204 NO CONTENT' )
      if $c->debug;

    $this->status_no_content( $c );

    return;
  }

  $c->stash->{rfamMaps} = \@mapping;

  # when specifically requested, or when the client will accept any format
  # ("*/*"), render as HTML
  if ( $c->req->accepts( 'text/xml' ) or
       $c->req->accepts( 'text/plain' )  or
       $c->req->accepts( 'application/json' )  ) {

    # for anything other than HTML, we need to serialise a data structure
    $c->log->debug( 'Family::structures: converting DBIC rows to perl data structure for serialisation' )
      if $c->debug;

    # load rows into a regular perl data structure
    foreach my $row ( @mapping ) {
      push @{ $c->stash->{rest}->{mapping} }, [
        $row->rfamseq_acc,
        $row->seq_start,
        $row->seq_end,
        $row->pdb_id,
        $row->chain || '',
        $row->pdb_res_start,
        $row->pdb_res_end,
      ];
    }

    # and pick a template
    $c->stash->{template} = $c->req->accepts( 'text/xml' )
                          ? 'rest/family/structure_mapping_xml.tt'
                          : 'rest/family/structure_mapping_text.tt';
  }
  else {
    # cache the template output for one week
    $c->cache_page( 604800 );

    # for HTML, we're dropping straight to a template
    $c->stash->{template} = 'components/blocks/family/structureTab.tt';
  }
}

#---------------------------------------

=head2 old_structures : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_structures : Path( '/family/structures/mapping' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_structures: redirecting to "structures"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'structures', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

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

1;


