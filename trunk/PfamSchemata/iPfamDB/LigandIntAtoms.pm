package iPfamDB::LigandIntAtoms;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ligand_int_atoms");
__PACKAGE__->add_columns(
  "internal_ligand_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "atom_number",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "ligand",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 4 },
  "atom_acc",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "atom",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 4 },
);
__PACKAGE__->set_primary_key("atom_acc");
__PACKAGE__->belongs_to(
  "internal_ligand_id",
  "iPfamDB::Ligands",
  { internal_ligand_id => "internal_ligand_id" },
);
__PACKAGE__->has_many(
  "protein_ligand_bonds",
  "iPfamDB::ProteinLigandBonds",
  { "foreign.ligand_atom" => "self.atom_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:rJD8Q+C1H94KQYjGrYGSpA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
