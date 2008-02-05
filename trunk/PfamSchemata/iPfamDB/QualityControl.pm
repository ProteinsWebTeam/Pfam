package iPfamDB::QualityControl;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("quality_control");
__PACKAGE__->add_columns(
  "quality_control",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "method",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "score",
  { data_type => "FLOAT", default_value => "", is_nullable => 0, size => 32 },
  "comment",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
);
__PACKAGE__->set_primary_key("quality_control");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ui0vKkT3t/zujjKjgtEjkQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
