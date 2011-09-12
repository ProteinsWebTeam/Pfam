package PfamLive::DeadFamilies;

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
  "title",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 1, size => 255 },
);
__PACKAGE__->add_unique_constraint("pfamA_acc", ["pfama_acc"]);
__PACKAGE__->set_primary_key("pfama_acc");


1;
