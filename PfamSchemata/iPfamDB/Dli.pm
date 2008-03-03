package iPfamDB::Dli;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("dli");
__PACKAGE__->add_columns(
  "dli",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "region_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "internal_ligand_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("dli");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:B2vNcmiB9S1p2142oLvllA


# You can replace this text with custom content, and it will be preserved on regeneration
__PACKAGE__->add_unique_constraint("dliConst", ["internal_ligand_id", "region_id"]);
1;
