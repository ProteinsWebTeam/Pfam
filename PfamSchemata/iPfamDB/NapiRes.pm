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
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "protein_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "residue",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "bond",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:qYsCruY1x7kQjKEqt6cMsw


# You can replace this text with custom content, and it will be preserved on regeneration
__PACKAGE__->add_unique_constraint("napiResConst", ["napi", "base", "residue", "bond"]);
__PACKAGE__->add_unique_constraint("napiResProtConst", ["nucleic_acid_acc", "base", "protein_acc", "residue", "bond"]);

1;
