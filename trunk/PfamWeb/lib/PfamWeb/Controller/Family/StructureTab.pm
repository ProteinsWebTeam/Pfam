
# StructureTab.pm
# jt6 20060411 WTSI
#
# $Id: StructureTab.pm,v 1.3 2007-03-15 14:06:10 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::StructureTab - controller for the
structure tab of the family pages.

=cut

package PfamWeb::Controller::Family::StructureTab;

=head1 DESCRIPTION

This controller retrieves the mapping between Pfam, UniProt and PDB
residues and hands off to a template that constructs the "structure"
tab of the family section.

$Id: StructureTab.pm,v 1.3 2007-03-15 14:06:10 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dumper;

use base "PfamWeb::Controller::Family";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 default : Path

Populates the stash with the mapping and hands off to the appropriate template.
Relies on the C<begin> method from the base class (Family) to get the Pfam 
accession, etc.

=cut

sub default : Path {
  my($this, $c) = @_;

  $c->log->debug( "Family::StructureTab::structureTab: acc: |"
		  . $c->stash->{acc}  . "|" .  $c->stash->{entryType}. "|");

  $c->forward( "_getMapping" );
  $c->stash->{template} = "components/blocks/family/structureTab.tt";
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
