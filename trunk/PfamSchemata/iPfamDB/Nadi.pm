package iPfamDB::Nadi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nadi");
__PACKAGE__->add_columns(
  "nadi",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "nucleic_acid_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("nadi");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Jyw4vx7Z2hVHuglT5Ly+Cg


# You can replace this text with custom content, and it will be preserved on regeneration
__PACKAGE__->add_unique_constraint("nadiConst", ["nucleic_acid_acc", "region_id"]);
1;
