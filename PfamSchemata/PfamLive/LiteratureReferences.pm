package PfamLive::LiteratureReferences;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("literature_references");
__PACKAGE__->add_columns(
  "auto_lit",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "pmid",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "title",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "author",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "journal",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
);
__PACKAGE__->set_primary_key("auto_lit");
__PACKAGE__->add_unique_constraint("IX_literature_references_1", ["pmid"]);
__PACKAGE__->has_many(
  "clan_lit_refs",
  "PfamLive::ClanLitRefs",
  { "foreign.auto_lit" => "self.auto_lit" },
);
__PACKAGE__->has_many(
  "pfama_literature_references",
  "PfamLive::PfamaLiteratureReferences",
  { "foreign.auto_lit" => "self.auto_lit" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:bGmyT+NZ+rHtl42XOTGNUQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
