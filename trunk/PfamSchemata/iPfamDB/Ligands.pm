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
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 20 },
);
__PACKAGE__->set_primary_key("internal_ligand_id");
__PACKAGE__->add_unique_constraint(
  "ligands_uniq_idx",
  ["accession", "ligand_id", "ligand_number", "chain"],
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:f+CT7ISNNAKTzhiWwlC4aQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
