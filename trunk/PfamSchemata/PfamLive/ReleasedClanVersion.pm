package PfamLive::ReleasedClanVersion;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("released_clan_version");
__PACKAGE__->add_columns(
  "auto_clan",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 4 },
  "desc_file",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 32 },
  "version",
  {
    data_type => "SMALLINT",
    default_value => undef,
    is_nullable => 1,
    size => 5,
  },
);
__PACKAGE__->belongs_to("auto_clan", "PfamLive::Clans", { auto_clan => "auto_clan" });


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-08-04 15:06:30
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:vwWRfTJzMrKxmRvGE/qk/Q

__PACKAGE__->set_primary_key("auto_clan");
# You can replace this text with custom content, and it will be preserved on regeneration
1;
