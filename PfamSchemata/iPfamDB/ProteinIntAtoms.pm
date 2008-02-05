package iPfamDB::ProteinIntAtoms;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("protein_int_atoms");
__PACKAGE__->add_columns(
  "protein_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 6 },
  "atom_number",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "protein_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
  "residue",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "atom_acc",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "atom",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 3 },
);
__PACKAGE__->set_primary_key("atom_acc");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3B5WOae+vneDOuSF2P4wzQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
