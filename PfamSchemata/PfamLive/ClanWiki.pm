package PfamLive::ClanWiki;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("clan_wiki");
__PACKAGE__->add_columns(
  "auto_clan",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 4 },
  "auto_wiki",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->belongs_to(
  "auto_wiki",
  "PfamLive::Wikipedia",
  { auto_wiki => "auto_wiki" },
);
__PACKAGE__->belongs_to("auto_clan", "PfamLive::Clans", { auto_clan => "auto_clan" });


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2010-03-23 10:27:02
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:pvtFU+QvPmQylZyWFL1MAg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
