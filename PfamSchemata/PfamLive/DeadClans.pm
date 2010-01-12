package PfamLive::DeadClans;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("dead_clans");
__PACKAGE__->add_columns(
  "clan_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
  "clan_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 40 },
  "clan_description",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "clan_membership",
  {
    data_type => "LONGTEXT",
    default_value => "",
    is_nullable => 0,
    size => 4294967295,
  },
  "comment",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "forward_to",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 6 },
  "user",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "killed",
  {
    data_type => "TIMESTAMP",
    default_value => "CURRENT_TIMESTAMP",
    is_nullable => 0,
    size => 14,
  },
);
__PACKAGE__->add_unique_constraint("clan_acc", ["clan_acc"]);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-07-24 17:53:29
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ZSucuua1lEXHIR0Yy6TUiA

__PACKAGE__->set_primary_key(qw(clan_acc));
# You can replace this text with custom content, and it will be preserved on regeneration
1;
