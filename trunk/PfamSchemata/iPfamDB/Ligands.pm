package iPfamDB::Ligands;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ligands");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "ligand_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 10,
  },
  "ligand_number",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "chain",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
  "atom_start",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "atom_end",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "internal_ligand_id",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("internal_ligand_id");
__PACKAGE__->add_unique_constraint(
  "ligands_uniq_idx",
  ["accession", "ligand_id", "ligand_number", "chain"],
);
__PACKAGE__->has_many(
  "dlis",
  "iPfamDB::Dli",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);
__PACKAGE__->has_many(
  "dli_res",
  "iPfamDB::DliRes",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);
__PACKAGE__->has_many(
  "ligand_int_atoms",
  "iPfamDB::LigandIntAtoms",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);
__PACKAGE__->belongs_to(
  "accession",
  "iPfamDB::PdbChainData",
  { "internal_chain_accession" => "accession" },
);
__PACKAGE__->belongs_to(
  "ligand_id",
  "iPfamDB::LigandChemistry",
  { ligand_id => "ligand_id" },
);
__PACKAGE__->has_many(
  "plis",
  "iPfamDB::Pli",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);
__PACKAGE__->has_many(
  "pli_res",
  "iPfamDB::PliRes",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3nItRCk1CS+9e0Cy+l1BEg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
