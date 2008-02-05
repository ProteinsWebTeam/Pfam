package iPfamDB::Nadi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nadi");
__PACKAGE__->add_columns(
  "nucleic_acid_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
  "base",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "base_type",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_residue",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "residue_type",
  { data_type => "BIGINT", default_value => "", is_nullable => 0, size => 20 },
  "quality_control",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->belongs_to("region_id", "iPfamDB::Domain", { region_id => "region_id" });


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Iu7JI4R+g94pq3/hzSiXaQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
