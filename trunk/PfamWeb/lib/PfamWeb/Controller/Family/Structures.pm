
# Structures.pm
# jt6 20060727 WTSI
#
# Controller to build an image of one of the PDB structure for the
# specified family, along with a form for choosing a different one
#
# $Id: Structures.pm,v 1.19 2010-01-19 09:46:06 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::Structures - controller to retrieve a Pdb
object for the specified PDB entry

=cut

package PfamWeb::Controller::Family::Structures;

=head1 DESCRIPTION

This controller can be called in two ways:

=over

=item * with just a Pfam family ID or accession, or

=item * with a Pfam ID/accession B<and> a PDB ID

=back

In the first case the action (L<default|"default : Private"> receives
a list of PDB IDs for this family via the "begin" method on the parent
class and hands off immediately to the template. The template then
chooses one of them at random and shows the image of that structure.

In the second case, when the action receives both a Pfam ID/accession
and a PDB ID, this controller retrieves the given Pdb object and hands
off to the template, which them displays the image for the specified
PDB entry. The only reason that we need to hand the controller both the
Pfam ID/accession and PDB ID is that the C<begin> method on the Family
parent class will complain otherwise.

Generates a B<page fragment>.

$Id: Structures.pm,v 1.19 2010-01-19 09:46:06 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Family';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 structures : Path

Retrieves the list of PDB entries for this family. If a PDB ID is specified,
the method also retrieves the row of the "pdb" table for that entry.

=cut

sub structures : Path :  Args(1) {
  my ( $this, $c ) = @_;

  # see if we were handed a valid PDB ID and, if so, just stash it
  if ( defined $c->req->param('pdbId') and
       $c->req->param('pdbId') =~ /^(\d\w{3})$/ ) {

    $c->log->debug( "Family::Structures::structures: got PDB ID: |$1|" )
      if $c->debug;

    $c->stash->{pdb_id} = $1;
  }

  # retrieve the PDB entries for this family
  my @regions;
  if ( defined $c->stash->{pfam}->auto_pfama ) {
    $c->log->debug( 'Family::Structures::structures: got an auto_pfama: '
                    . $c->stash->{pfam}->auto_pfama ) if $c->debug;
    @regions = $c->model('PfamDB::PdbPfamaReg')
                 ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama },
                           { prefetch   => [ qw( pdb_id pdb_image ) ] } );
    $c->log->debug( 'Family::Structures::structures: got ' 
                    . scalar @regions . ' regions' ) if $c->debug;
  }

  # don't render the template unless we need to
  unless ( scalar @regions ) {
    $c->log->debug( 'Family::Structures::structures: no structure image; not rendering template' )
      if $c->debug;
    $c->res->status( 204 );
    return;
  }

  my $pdb_unique = {};
  my $colours = {};
  foreach my $region ( @regions ) {
    my $id = $region->pdb_id->pdb_id;
    $pdb_unique->{$id} = $region;
    $colours->{$id}->{$region->hex_colour} = $region->auto_pfama->pfama_id;
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

#-------------------------------------------------------------------------------

=head2 mapping : Local

Renders a table showing the mapping between Pfam family, UniProt region and
PDB residues.

=cut

sub mapping : Local  {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::Structures::mapping: acc: |'
                  . $c->stash->{acc}  . '|' .  $c->stash->{entryType}. '|' )
    if $c->debug;

  my @mapping = $c->model('PfamDB::PdbPfamaReg')
                  ->search( { auto_pfama => $c->stash->{pfam}->auto_pfama },
                            { join       => [ qw( pdb_id auto_pfamseq ) ],
                              columns    => [ qw( auto_pfamseq.pfamseq_id
                                                  seq_start
                                                  seq_end
                                                  pdb_id.pdb_id
                                                  chain
                                                  pdb_res_start
                                                  pdb_res_end ) ] } );

  $c->stash->{pfamMaps} = \@mapping;
  $c->log->debug( 'Family::Structures::mapping: found |' . scalar @mapping . '| rows' )
    if $c->debug;

  unless ( scalar @mapping ) {
    $c->log->debug( 'Family::Structures::mapping: no rows; returning 204' )
      if $c->debug;
      
    $c->res->status( 204 );
    
    return;
  }
  
  if ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Family::Structures::mapping: emitting XML' ) if $c->debug;
    $c->stash->{template} = 'rest/family/structures_xml.tt';
  }
  else {
    $c->log->debug( 'Family::Structures::mapping: emitting HTML' ) if $c->debug;
    $c->stash->{template} = 'components/blocks/family/structureTab.tt';
  }

  # cache the template output for one week
  $c->cache_page( 604800 );
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
