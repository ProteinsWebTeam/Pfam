package iPfamDB::Nardi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nardi");
__PACKAGE__->add_columns(
  "nardi",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "protein_region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "nucleic_region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("nardi");
__PACKAGE__->belongs_to(
  "nucleic_region_id",
  "iPfamDB::NucleicAcidRegion",
  { region_id => "nucleic_region_id" },
);
__PACKAGE__->belongs_to(
  "protein_region_id",
  "iPfamDB::Domain",
  { region_id => "protein_region_id" },
);
__PACKAGE__->has_many(
  "nardi_res",
  "iPfamDB::NardiRes",
  { "foreign.nardi" => "self.nardi" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:qKWxAfVnFSFk51S2fxI2lA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
