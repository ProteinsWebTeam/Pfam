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
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_residue",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "ligand_residue",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "bond",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
);
__PACKAGE__->belongs_to(
  "internal_ligand_id",
  "iPfamDB::Ligands",
  { internal_ligand_id => "internal_ligand_id" },
);
__PACKAGE__->belongs_to("region_id", "iPfamDB::Domain", { region_id => "region_id" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:XdyR2tuj0+pIof0nrIxPQw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
