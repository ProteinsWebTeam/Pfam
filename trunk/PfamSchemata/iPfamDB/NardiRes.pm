package iPfamDB::NardiRes;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nardi_res");
__PACKAGE__->add_columns(
  "nardi",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "protein_region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "nucleic_region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "protein_residue",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "nucleic_base",
  { data_type => "BIGINT", default_value => "", is_nullable => 0, size => 19 },
  "quality_control",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->belongs_to("nardi", "iPfamDB::Nardi", { nardi => "nardi" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ItaRuQtRbmWX5+c5QNl92w


# You can replace this text with custom content, and it will be preserved on regeneration
1;
