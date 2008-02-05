package iPfamDB::NucleicAcidIntAtoms;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nucleic_acid_int_atoms");
__PACKAGE__->add_columns(
  "nucleic_acid_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 6 },
  "atom_number",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "nucleic_acid_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
  "base",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "base_name",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "atom",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 3 },
  "atom_acc",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3MtAbiicvEIUd18in6munw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
