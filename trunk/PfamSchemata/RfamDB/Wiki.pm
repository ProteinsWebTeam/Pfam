package RfamDB::Wiki;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("wiki");
__PACKAGE__->add_columns(
  "auto_wiki",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "auto_rfam",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "rfam_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
  "title",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 150 },
);
__PACKAGE__->belongs_to("auto_rfam", "RfamDB::Rfam", { auto_rfam => "auto_rfam" });
__PACKAGE__->belongs_to("auto_wiki", "RfamDB::Wikitext", { auto_wiki => "auto_wiki" });


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-14 20:19:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:p/1RFouF5MD0yajNlCZogg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
