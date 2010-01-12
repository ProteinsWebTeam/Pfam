package PfamLive::ClanMembership;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("clan_membership");
__PACKAGE__->add_columns(
  "auto_clan",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 4 },
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
);
__PACKAGE__->belongs_to("auto_clan", "PfamLive::Clans", { auto_clan => "auto_clan" });
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:u4W7m2GV9KpECd/4L0JpGg
__PACKAGE__->set_primary_key(qw(auto_clan auto_pfama));
__PACKAGE__->add_unique_constraint("clanMembConst", [qw(auto_clan auto_pfama)]);
# You can replace this text with custom content, and it will be preserved on regeneration
1;
