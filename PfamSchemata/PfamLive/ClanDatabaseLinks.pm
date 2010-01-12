package PfamLive::ClanDatabaseLinks;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("clan_database_links");
__PACKAGE__->add_columns(
  "auto_clan",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 4 },
  "db_id",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 0,
    size => 255,
  },
  "comment",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "db_link",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 0,
    size => 255,
  },
  "other_params",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
);
__PACKAGE__->belongs_to("auto_clan", "PfamLive::Clans", { auto_clan => "auto_clan" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:J3Lv14/WLZ9bz+8VS7EbhA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
