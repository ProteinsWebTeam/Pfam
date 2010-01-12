package PfamLive::ClanVersions;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("clan_versions");
__PACKAGE__->add_columns(
  "auto_clan",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 4 },
  "version",
  { data_type => "TINYINT", default_value => undef, is_nullable => 1, size => 4 },
  "rcs_trace",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 0,
    size => 4294967295,
  },
  "message",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "user",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 0,
    size => 255,
  },
  "updated",
  {
    data_type => "TIMESTAMP",
    default_value => "CURRENT_TIMESTAMP",
    is_nullable => 0,
    size => 14,
  },
);
__PACKAGE__->belongs_to("auto_clan", "PfamLive::Clans", { auto_clan => "auto_clan" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:2l8J57cFYcUZGW2on4YQeQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
