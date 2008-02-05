package iPfamDB::LigandIntAtoms;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ligand_int_atoms");
__PACKAGE__->add_columns(
  "internal_ligand_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "atom_number",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "residue",
  { data_type => "BIGINT", default_value => "", is_nullable => 0, size => 20 },
  "atom_acc",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "atom",
  { data_type => "BIGINT", default_value => "", is_nullable => 0, size => 20 },
);
__PACKAGE__->set_primary_key("atom_acc");
__PACKAGE__->belongs_to(
  "internal_ligand_id",
  "iPfamDB::Ligands",
  { internal_ligand_id => "internal_ligand_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:p0uYLnz33+vU/qKT2Sqopw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
