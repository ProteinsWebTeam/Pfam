package iPfamDB::Rfam;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("rfam");
__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 8 },
  "rfam_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "decsription",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
);
__PACKAGE__->set_primary_key("rfam_acc");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:UmpLKUFJuwdqY5O8IRXOag


# You can replace this text with custom content, and it will be preserved on regeneration
1;
