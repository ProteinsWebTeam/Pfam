package RfamDB::ClanMembership;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("clan_membership");
__PACKAGE__->add_columns(
  "auto_clan",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "auto_rfam",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
);
__PACKAGE__->belongs_to("auto_clan", "RfamDB::Clans", { auto_clan => "auto_clan" });
__PACKAGE__->belongs_to("auto_rfam", "RfamDB::Rfam", { auto_rfam => "auto_rfam" });


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2010-01-12 10:09:30
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:bDkm1rBrAyuqCFU+IE65Pg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
