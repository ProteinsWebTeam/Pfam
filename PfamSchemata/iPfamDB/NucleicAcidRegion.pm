package iPfamDB::NucleicAcidRegion;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nucleic_acid_region");
__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 8 },
  "accession",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 6 },
  "start",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "end",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_source_db",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
  "nucleic_acid_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
);
__PACKAGE__->set_primary_key("region_id");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:jYgV/kJR+oA2rDSHHYlFDQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
