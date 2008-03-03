package iPfamDB::Napi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("napi");
__PACKAGE__->add_columns(
  "napi",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "nucleic_acid_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "protein_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
);
__PACKAGE__->set_primary_key("napi");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:VXrlD9EoWv89AwtYaR5PsA


# You can replace this text with custom content, and it will be preserved on regeneration

__PACKAGE__->add_unique_constraint("napiConst", ["protein_acc", "nucleic_acid_acc"]);
1;
