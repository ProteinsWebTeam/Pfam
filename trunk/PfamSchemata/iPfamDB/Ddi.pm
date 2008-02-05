package iPfamDB::Ddi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ddi");
__PACKAGE__->add_columns(
  "region_id_a",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_id_b",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "residue_a",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "residue_b",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "intrachain",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 4 },
  "quality_control",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->belongs_to(
  "region_id_b",
  "iPfamDB::Domain",
  { region_id => "region_id_b" },
);
__PACKAGE__->belongs_to(
  "region_id_a",
  "iPfamDB::Domain",
  { region_id => "region_id_a" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ozYm7DzgJqfkkX+kttbPgg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
