package iPfamDB::DliRes;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("dli_res");
__PACKAGE__->add_columns(
  "dli",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "internal_ligand_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "region_residue",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "ligand_residue",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "bond",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:B3jHZaHgvx9p44YTHb9zOA


# You can replace this text with custom content, and it will be preserved on regeneration
__PACKAGE__->add_unique_constraint("dliConst", ["dli", "region_residue", "ligand_residue", "bond"]);
__PACKAGE__->add_unique_constraint("dliProtConst", ["internal_ligand_id", "region_id", "region_residue", "ligand_residue", "bond"]);
1;
