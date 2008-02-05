package iPfamDB::Nardi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nardi");
__PACKAGE__->add_columns(
  "protein_region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "nucleic_region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "protein_residue",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "nucleic_base",
  { data_type => "BIGINT", default_value => "", is_nullable => 0, size => 20 },
  "quality_control",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:eAR7UY1nFqZhVKkGf9lP4Q


# You can replace this text with custom content, and it will be preserved on regeneration
1;
