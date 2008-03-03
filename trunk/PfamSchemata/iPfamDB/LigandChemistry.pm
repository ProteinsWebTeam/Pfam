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
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 5 },
  "name",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "systematic_name",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "num_all_atoms",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "num_atoms_no_h",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "stereo_smiles",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "non_stereo_smiles",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "charge",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 11 },
  "category",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "formula",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "molecular_weight",
  { data_type => "FLOAT", default_value => undef, is_nullable => 1, size => 32 },
);
__PACKAGE__->add_unique_constraint("ligand_chemistry_ligand_id_Idx", ["ligand_id"]);
__PACKAGE__->add_unique_constraint("ligand_chemistry_code_Idx", ["lig_code"]);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:30W8zCkNvzz3IgHdy4L3AA


# You can replace this text with custom content, and it will be preserved on regeneration

__PACKAGE__->add_unique_constraint("ligand_chemistry_three_letter_code", ["three_letter_code"]);
1;
