package PfamLive::Lock;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("_lock");
__PACKAGE__->add_columns(
  "locked",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 1 },
  "locker",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "allowcommits",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 1 },
  "alsoallow",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-08-04 15:06:30
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ngJeFLlc7ha//f0rDVutKw

__PACKAGE__->set_primary_key("locker");

# You can replace this text with custom content, and it will be preserved on regeneration
1;
