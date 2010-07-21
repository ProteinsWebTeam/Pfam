package iPfamDB::NapiRes;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("napi_res");
__PACKAGE__->add_columns(
  "napi",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "nucleic_acid_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "base",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "protein_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "residue",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "bond",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3eSO0pkAJ081HtvGa7TD7Q


# You can replace this text with custom content, and it will be preserved on regeneration
1;
