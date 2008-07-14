package RfamDB::Wikitext;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("wikitext");
__PACKAGE__->add_columns(
  "auto_wiki",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "text",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
);
__PACKAGE__->set_primary_key("auto_wiki");
__PACKAGE__->has_many(
  "wikis",
  "RfamDB::Wiki",
  { "foreign.auto_wiki" => "self.auto_wiki" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-14 20:19:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Lj4WKLRVnzMIFXflow4raw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
