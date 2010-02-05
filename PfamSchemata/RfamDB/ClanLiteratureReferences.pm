package RfamDB::ClanLiteratureReferences;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("clan_literature_references");
__PACKAGE__->add_columns(
  "auto_clan",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_lit",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "comment",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "order_added",
  { data_type => "TINYINT", default_value => undef, is_nullable => 1, size => 3 },
);
__PACKAGE__->belongs_to(
  "auto_lit",
  "RfamDB::LiteratureReferences",
  { auto_lit => "auto_lit" },
);
__PACKAGE__->belongs_to("auto_clan", "RfamDB::Clans", { auto_clan => "auto_clan" });


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2010-01-12 10:09:30
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:XEHEaK0qXP1M34qChFoj5g


# You can replace this text with custom content, and it will be preserved on regeneration
1;
