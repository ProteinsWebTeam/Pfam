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
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "residue_b",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "intrachain",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 1 },
  "bond",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:TuCGvHcfl9Z4eIqvJGPCjg


# You can replace this text with custom content, and it will be preserved on regeneration
__PACKAGE__->add_unique_constraint("ppiResConst", ["ddi", "residue_a", "residue_b", "intrachain", "bond"]);
__PACKAGE__->add_unique_constraint("ppiResProtConst", ["region_id_a", "region_id_b", "residue_a", "residue_b", "intrachain", "bond"]);
1;
