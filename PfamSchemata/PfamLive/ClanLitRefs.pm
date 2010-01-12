package PfamLive::ClanLitRefs;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("clan_lit_refs");
__PACKAGE__->add_columns(
  "auto_clan",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 4 },
  "auto_lit",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "order_added",
  { data_type => "TINYINT", default_value => undef, is_nullable => 0, size => 4 },
  "comment",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
);
__PACKAGE__->belongs_to("auto_clan", "PfamLive::Clans", { auto_clan => "auto_clan" });
__PACKAGE__->belongs_to(
  "auto_lit",
  "PfamLive::LiteratureReferences",
  { auto_lit => "auto_lit" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:5RrW6m8xaKN+rNYY2PYBag


# You can replace this text with custom content, and it will be preserved on regeneration
1;
