package iPfamDB::Pfama;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfama");
__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 8 },
  "pfama_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "numberinalign",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "description",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
);
__PACKAGE__->set_primary_key("pfama_acc");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:0J5+9nwjyJrY8lIDl1JIhw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
