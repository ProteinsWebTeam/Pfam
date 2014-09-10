package RfamDB::Result::Keyword;

use strict;
use warnings;

use base 'DBIx::Class::Core';

__PACKAGE__->table("keywords");
__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 7 },
  "rfam_id",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 40, },
  "description",
  { data_type => "VARCHAR", default_value => "NULL", is_nullable => 1, size => 100, },
  "rfam_general",
  { data_type => "LONGTEXT", default_value => undef, is_nullable => 1, size => 4294967295, },
  "literature",
  { data_type => "LONGTEXT", default_value => undef, is_nullable => 1, size => 4294967295, },
  "wiki",
  { data_type => "LONGTEXT", default_value => undef, is_nullable => 1, size => 4294967295, },
  "pdb_mappings",
  { data_type => "LONGTEXT", default_value => undef, is_nullable => 1, size => 4294967295, },
  "clan_info",
  { data_type => "LONGTEXT", default_value => undef, is_nullable => 1, size => 4294967295, },
);

__PACKAGE__->set_primary_key("rfam_acc");

__PACKAGE__->belongs_to(
  "rfam_acc",
  "RfamDB::Result::Family",
  { rfam_acc => "rfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

1;
