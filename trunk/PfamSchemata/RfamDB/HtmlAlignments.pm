package RfamDB::HtmlAlignments;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("html_alignments");
__PACKAGE__->add_columns(
  "auto_rfam",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "type",
  { data_type => "ENUM", default_value => "", is_nullable => 0, size => 4 },
  "html",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "block",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 6 },
);
__PACKAGE__->set_primary_key("auto_rfam", "type", "block");
__PACKAGE__->belongs_to("auto_rfam", "RfamDB::Rfam", { auto_rfam => "auto_rfam" });


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-15 13:36:20
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:2d1XsqnhYoWFEKAU/8R73A


# You can replace this text with custom content, and it will be preserved on regeneration
1;
