package iPfamDB::QualityControl;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("quality_control");
__PACKAGE__->add_columns(
  "quality_control",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "int_type",
  { data_type => "ENUM", default_value => undef, is_nullable => 1, size => 5 },
  "method",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "score",
  { data_type => "FLOAT", default_value => undef, is_nullable => 1, size => 32 },
  "comment",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
);
__PACKAGE__->add_unique_constraint("UQ_quality_control_1", ["quality_control", "int_type"]);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:AaDxX8X2+Hxfsx/e2vnk8g


# You can replace this text with custom content, and it will be preserved on regeneration
1;
