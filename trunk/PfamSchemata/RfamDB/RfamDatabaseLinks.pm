package RfamDB::RfamDatabaseLinks;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("rfam_database_links");
__PACKAGE__->add_columns(
  "auto_rfam",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "db_id",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "comment",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "db_link",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "other_params",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
);
__PACKAGE__->belongs_to("auto_rfam", "RfamDB::Rfam", { auto_rfam => "auto_rfam" });


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-14 20:19:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:kbvDrKFzg/sVKCMRxjny2g


# You can replace this text with custom content, and it will be preserved on regeneration
1;
