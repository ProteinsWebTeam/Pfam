package PfamDB::Clans;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("clans");
__PACKAGE__->add_columns(
  "auto_clan",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 4 },
  "clan_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 6 },
  "clan_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 40 },
  "previous_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 75,
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
  "deposited_by",
  {
    data_type => "VARCHAR",
    default_value => "anon",
    is_nullable => 0,
    size => 100,
  },
  "clan_comment",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "updated",
  {
    data_type => "TIMESTAMP",
    default_value => "CURRENT_TIMESTAMP",
    is_nullable => 0,
    size => 14,
  },
  "created",
  {
    data_type => "DATETIME",
    default_value => undef,
    is_nullable => 1,
    size => 19,
  },
  "version",
  {
    data_type => "SMALLINT",
    default_value => undef,
    is_nullable => 1,
    size => 5,
  },
  "number_structures",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "number_archs",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "number_species",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "number_sequences",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "competed",
  { data_type => "TINYINT", default_value => undef, is_nullable => 1, size => 1 },
);
__PACKAGE__->set_primary_key("auto_clan");
__PACKAGE__->add_unique_constraint("clan_id", ["clan_id"]);
__PACKAGE__->add_unique_constraint("clan_acc", ["clan_acc"]);
__PACKAGE__->has_many(
  "clan_alignments_and_relationships",
  "PfamDB::ClanAlignmentsAndRelationships",
  { "foreign.auto_clan" => "self.auto_clan" },
);
__PACKAGE__->has_many(
  "clan_architectures",
  "PfamDB::ClanArchitecture",
  { "foreign.auto_clan" => "self.auto_clan" },
);
__PACKAGE__->has_many(
  "clan_database_links",
  "PfamDB::ClanDatabaseLinks",
  { "foreign.auto_clan" => "self.auto_clan" },
);
__PACKAGE__->has_many(
  "clan_lit_refs",
  "PfamDB::ClanLitRefs",
  { "foreign.auto_clan" => "self.auto_clan" },
);
__PACKAGE__->has_many(
  "clan_memberships",
  "PfamDB::ClanMembership",
  { "foreign.auto_clan" => "self.auto_clan" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-07-24 17:53:29
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:jZbO4jxrO72Hm7SP+C3Mcw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
