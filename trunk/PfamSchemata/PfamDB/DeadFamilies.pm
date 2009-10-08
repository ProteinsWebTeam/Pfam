package PfamDB::DeadFamilies;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("dead_families");
__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
  "pfama_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 40 },
  "comment",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "forward_to",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 7 },
  "user",
  {
    data_type => "VARCHAR",
    default_value => "anon",
    is_nullable => 0,
    size => 10,
  },
  "killed",
  {
    data_type => "TIMESTAMP",
    default_value => "CURRENT_TIMESTAMP",
    is_nullable => 0,
    size => 14,
  },
);
__PACKAGE__->add_unique_constraint("pfamA_acc", ["pfama_acc"]);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-07-24 17:53:29
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3N/eAyuLsymFIg7SmhN79g


# You can replace this text with custom content, and it will be preserved on regeneration
1;
