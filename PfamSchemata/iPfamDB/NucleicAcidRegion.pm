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
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "end",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_source_db",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
  "nucleic_acid_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
);
__PACKAGE__->set_primary_key("region_id");
__PACKAGE__->has_many(
  "nardis",
  "iPfamDB::Nardi",
  { "foreign.nucleic_region_id" => "self.region_id" },
);
__PACKAGE__->belongs_to("rfam_acc", "iPfamDB::Rfam", { rfam_acc => "rfam_acc" });
__PACKAGE__->belongs_to(
  "accession",
  "iPfamDB::NucleicAcid",
  { accession => "accession" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:XkPN4kCB1pXw6cKmXrsTRg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
