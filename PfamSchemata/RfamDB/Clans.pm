package RfamDB::Clans;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("clans");
__PACKAGE__->add_columns(
  "auto_clan",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "clan_acc",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 7 },
  "clan_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 40,
  },
  "clan_description",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "clan_author",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "clan_comment",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
);
__PACKAGE__->set_primary_key("auto_clan");
__PACKAGE__->has_many(
  "clan_database_links",
  "RfamDB::ClanDatabaseLinks",
  { "foreign.auto_clan" => "self.auto_clan" },
);
__PACKAGE__->has_many(
  "clan_literature_references",
  "RfamDB::ClanLiteratureReferences",
  { "foreign.auto_clan" => "self.auto_clan" },
);
__PACKAGE__->has_many(
  "clan_memberships",
  "RfamDB::ClanMembership",
  { "foreign.auto_clan" => "self.auto_clan" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2010-01-12 10:09:30
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:yYUAyj2T5vfmNaDofSUy2A


# You can replace this text with custom content, and it will be preserved on regeneration
1;
