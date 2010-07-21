package iPfamDB::ProteinLigandBonds;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("protein_ligand_bonds");
__PACKAGE__->add_columns(
  "protein_atom",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "ligand_atom",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "bond_type",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
  "distance",
  { data_type => "FLOAT", default_value => "", is_nullable => 0, size => 32 },
  "atom_acc",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->add_unique_constraint(
  "protein_ligand_bonds_unique",
  ["protein_atom", "ligand_atom", "bond_type"],
);
__PACKAGE__->belongs_to(
  "protein_atom",
  "iPfamDB::ProteinIntAtoms",
  { atom_acc => "protein_atom" },
);
__PACKAGE__->belongs_to(
  "ligand_atom",
  "iPfamDB::LigandIntAtoms",
  { atom_acc => "ligand_atom" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:G8ebEcCv/rSLwV1BBgwJ6w


# You can replace this text with custom content, and it will be preserved on regeneration
1;
