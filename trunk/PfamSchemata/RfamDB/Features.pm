package RfamDB::Features;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("features");
__PACKAGE__->add_columns(
  "auto_features",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "auto_rfamseq",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "database_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
  "primary_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 100 },
  "secondary_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "feat_orient",
  { data_type => "TINYINT", default_value => 0, is_nullable => 0, size => 3 },
  "feat_start",
  { data_type => "BIGINT", default_value => 0, is_nullable => 0, size => 19 },
  "feat_end",
  { data_type => "BIGINT", default_value => 0, is_nullable => 0, size => 19 },
  "quaternary_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 150,
  },
);
__PACKAGE__->set_primary_key("auto_features");


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2010-01-12 10:09:30
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:PhU6+85OFVby+1EpQt0LKA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
