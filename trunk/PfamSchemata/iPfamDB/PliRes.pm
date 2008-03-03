package iPfamDB::PliRes;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pli_res");
__PACKAGE__->add_columns(
  "pli",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "protein_acc",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "internal_ligand_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "residue_a",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "residue_b",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "bond",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:mdkgWbalHgWzxA9nGnlkqA


# You can replace this text with custom content, and it will be preserved on regeneration

__PACKAGE__->add_unique_constraint(
      "pliResConst" => ["pli", "residue_a", "residue_b", "bond"],
      "pliResProtConst" => ["protein_acc", "internal_ligand_id", "residue_a", "residue_b", "bond"]
      );
1;
