package iPfamDB::Ligands;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ligands");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "ligand_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "ligand_number",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "chain",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
  "atom_start",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "atom_end",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "internal_ligand_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("internal_ligand_id");
__PACKAGE__->has_many(
  "dlis",
  "iPfamDB::Dli",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);
__PACKAGE__->has_many(
  "ligand_int_atoms",
  "iPfamDB::LigandIntAtoms",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);
__PACKAGE__->belongs_to("accession", "iPfamDB::Pdb", { pdb_id => "accession" });
__PACKAGE__->belongs_to(
  "ligand_id",
  "iPfamDB::LigandChemistry",
  { ligand_id => "ligand_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:qLM38Gk9MqkZKjEUeoOVWw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
