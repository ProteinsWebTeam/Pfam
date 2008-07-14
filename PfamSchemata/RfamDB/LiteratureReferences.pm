package RfamDB::LiteratureReferences;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("literature_references");
__PACKAGE__->add_columns(
  "auto_lit",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "medline",
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
__PACKAGE__->has_many(
  "rfam_literature_references",
  "RfamDB::RfamLiteratureReferences",
  { "foreign.auto_lit" => "self.auto_lit" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-14 20:19:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:rjrE58VJPEfKyb8rye9MEg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
