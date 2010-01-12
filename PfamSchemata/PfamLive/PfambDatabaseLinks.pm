package PfamLive::PfambDatabaseLinks;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamB_database_links");
__PACKAGE__->add_columns(
  "auto_pfamb",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 6 },
  "db_id",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 0,
    size => 255,
  },
  "comment",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "db_link",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 0,
    size => 255,
  },
  "other_params",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
);
__PACKAGE__->belongs_to(
  "auto_pfamb",
  "PfamLive::Pfamb",
  { auto_pfamb => "auto_pfamb" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:46y0EGcrgY44ojeTBOTwXg
__PACKAGE__->set_primary_key("auto_pfamb");

# You can replace this text with custom content, and it will be preserved on regeneration
1;
