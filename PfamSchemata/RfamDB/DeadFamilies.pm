package RfamDB::DeadFamilies;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("dead_families");
__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
  "comment",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "forward_to",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 7 },
);
__PACKAGE__->set_primary_key("rfam_acc");


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-14 20:19:36
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:99N+4ne+lerWYeSNyHE2+Q


# You can replace this text with custom content, and it will be preserved on regeneration
1;
