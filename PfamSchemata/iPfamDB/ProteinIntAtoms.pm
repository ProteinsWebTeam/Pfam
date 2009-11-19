package iPfamDB::ProteinIntAtoms;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("protein_int_atoms");
__PACKAGE__->add_columns(
  "protein_acc",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "atom_number",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "protein_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "residue",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "atom_acc",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "atom",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 3 },
);
__PACKAGE__->set_primary_key("atom_acc");
__PACKAGE__->belongs_to(
  "protein_acc",
  "iPfamDB::Protein",
  { accession => "protein_acc" },
);
__PACKAGE__->has_many(
  "protein_ligand_bonds",
  "iPfamDB::ProteinLigandBonds",
  { "foreign.protein_atom" => "self.atom_acc" },
);
__PACKAGE__->has_many(
  "protein_nucleic_acid_bonds",
  "iPfamDB::ProteinNucleicAcidBonds",
  { "foreign.protein_atom" => "self.atom_acc" },
);
__PACKAGE__->has_many(
  "protein_protein_bonds",
  "iPfamDB::ProteinProteinBonds",
  { "foreign.atom_a" => "self.atom_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.04006 @ 2009-11-16 12:00:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:6IzTWlHo7uudJmOrcEByxA


# You can replace this text with custom content, and it will be preserved on regeneration

=head1 AUTHOR

Prasad Gunasekaran, C<pg6@sanger.ac.uk>

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
