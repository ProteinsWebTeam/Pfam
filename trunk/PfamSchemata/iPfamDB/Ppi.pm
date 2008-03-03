package iPfamDB::Ppi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ppi");
__PACKAGE__->add_columns(
  "ppi",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "protein_acc_a",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "protein_acc_b",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
);
__PACKAGE__->set_primary_key("ppi");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3/00JOAbTmovwB6OUQnFBQ


# You can replace this text with custom content, and it will be preserved on regeneration
__PACKAGE__->add_unique_constraint("ppiConst", ["protein_acc_a", "protein_acc_b"]);

1;
