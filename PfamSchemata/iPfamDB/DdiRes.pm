package iPfamDB::DdiRes;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ddi_res");
__PACKAGE__->add_columns(
  "ddi",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_id_a",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_id_b",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "residue_a",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "residue_b",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "intrachain",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 3 },
  "bond",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
);
__PACKAGE__->belongs_to("ddi", "iPfamDB::Ddi", { ddi => "ddi" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:YZEVLBSrmgdBktbpo3z5kw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
