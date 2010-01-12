package PfamLive::ClanArchitecture;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("clan_architecture");
__PACKAGE__->add_columns(
  "auto_clan",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 4 },
  "auto_architecture",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
);
__PACKAGE__->belongs_to("auto_clan", "PfamLive::Clans", { auto_clan => "auto_clan" });
__PACKAGE__->belongs_to(
  "auto_architecture",
  "PfamLive::Architecture",
  { auto_architecture => "auto_architecture" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:XMIrh3L75HOxD5AKL5z0vA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
