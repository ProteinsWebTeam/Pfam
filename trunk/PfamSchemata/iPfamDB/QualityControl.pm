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


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:KKB+eZTnyu6peEw1yqD11A


# You can replace this text with custom content, and it will be preserved on regeneration
__PACKAGE__->add_unique_constraint(
    qcConst => [ qw(quality_control int_type method comment) ],
);
1;
