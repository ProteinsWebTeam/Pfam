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
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "internal_ligand_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("dli");
__PACKAGE__->belongs_to(
  "internal_ligand_id",
  "iPfamDB::Ligands",
  { internal_ligand_id => "internal_ligand_id" },
);
__PACKAGE__->belongs_to("region_id", "iPfamDB::Domain", { region_id => "region_id" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:N2XvY1VKhnp9ny9+EOq4OQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
