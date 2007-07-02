
# Structures.pm
# jt6 20060727 WTSI
#
# Controller to build an image of one of the PDB structure for the
# specified family, along with a form for choosing a different one
#
# $Id: Structures.pm,v 1.7 2007-07-02 09:45:29 jt6 Exp $

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

$Id: Structures.pm,v 1.7 2007-07-02 09:45:29 jt6 Exp $

=cut

use strict;
use warnings;

use base "PfamWeb::Controller::Family";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 default : Private

Captures a URL like
http://localhost:3000/family/structures?acc=PF00067&amp;pdbId=1w9h 

=cut

sub default : Private {
  my( $this, $c ) = @_;

  my $id;
  ( $id ) = $c->req->param('pdbId') =~ /^(\d\w{3})$/
    if defined $c->req->param('pdbId');

  $c->stash->{pdbObj} = $c->model('PfamDB::Pdb')
                          ->find( { pdb_id => $id } )
    if defined $id;

  my @rs;
  if( defined $c->stash->{pfam}->auto_pfamA ) {
    @rs = $c->model('PfamDB::Pdb_pfamA_reg')
            ->search( { auto_pfamA => $c->stash->{pfam}->auto_pfamA},
                      { join       => [ qw( pdb ) ],
                        prefetch   => [ qw( pdb ) ] } );
  }

  my %pdbUnique = map{ $_->pdb_id => $_ } @rs;
  $c->stash->{pdbUnique} = \%pdbUnique;

  # set up the view and rely on "end" from the parent class to render it
  $c->stash->{template} = 'components/blocks/family/familyStructures.tt';
}

#-------------------------------------------------------------------------------

=head2 structureTab : Path

Populates the stash with the mapping and hands off to the appropriate template.

=cut

sub structureTab : Path('/family/structuretab')  {
  my($this, $c) = @_;

  $c->log->debug( 'Family::Structures::structureTab: acc: |'
                  . $c->stash->{acc}  . '|' .  $c->stash->{entryType}. '|' );

  my @mapping = $c->model('PfamDB::PdbMap')
                  ->search( { auto_pfam   => $c->stash->{pfam}->auto_pfamA,
                              pfam_region => 1 },
                            { join        => [ qw( pdb ) ],
                              prefetch    => [ qw( pdb ) ]
                            } );
  $c->stash->{pfamMaps} = \@mapping;

  $c->stash->{template} = 'components/blocks/family/structureTab.tt';
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
