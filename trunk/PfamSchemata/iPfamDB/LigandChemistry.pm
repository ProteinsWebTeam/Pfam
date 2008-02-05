package iPfamDB::LigandChemistry;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ligand_chemistry");
__PACKAGE__->add_columns(
  "ligand_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "lig_code",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "three_letter_code",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "one_letter_code",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "name",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "systematic_name",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "num_all_atoms",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "num_atoms_no_h",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "stereo_smiles",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "non_stereo_smiles",
  { data_type => "BIGINT", default_value => "", is_nullable => 0, size => 20 },
  "charge",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "category",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "formula",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "molecular_weight",
  { data_type => "FLOAT", default_value => "", is_nullable => 0, size => 32 },
);
__PACKAGE__->add_unique_constraint("ligand_chemistry_ligand_id_Idx", ["ligand_id"]);
__PACKAGE__->add_unique_constraint("ligand_chemistry_code_Idx", ["lig_code"]);
__PACKAGE__->has_many(
  "ligand_synonyms",
  "iPfamDB::LigandSynonyms",
  { "foreign.ligand_id" => "self.ligand_id" },
);
__PACKAGE__->has_many(
  "ligands",
  "iPfamDB::Ligands",
  { "foreign.ligand_id" => "self.ligand_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:L23n864yqivgvFNGdATy2Q


# You can replace this text with custom content, and it will be preserved on regeneration
1;
