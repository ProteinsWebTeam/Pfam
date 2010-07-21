package iPfamDB::Domain;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("domain");
__PACKAGE__->add_columns(
  "pfam_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 8 },
  "protein_accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "start",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "end",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_id",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "region_source_db",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
  "protein_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
);
__PACKAGE__->set_primary_key("region_id");
__PACKAGE__->has_many(
  "ddi_region_id_as",
  "iPfamDB::Ddi",
  { "foreign.region_id_a" => "self.region_id" },
);
__PACKAGE__->has_many(
  "ddi_region_id_bs",
  "iPfamDB::Ddi",
  { "foreign.region_id_b" => "self.region_id" },
);
__PACKAGE__->has_many(
  "dlis",
  "iPfamDB::Dli",
  { "foreign.region_id" => "self.region_id" },
);
__PACKAGE__->has_many(
  "dli_res",
  "iPfamDB::DliRes",
  { "foreign.region_id" => "self.region_id" },
);
__PACKAGE__->belongs_to("pfam_acc", "iPfamDB::Pfama", { pfama_acc => "pfam_acc" });
__PACKAGE__->has_many(
  "nadis",
  "iPfamDB::Nadi",
  { "foreign.region_id" => "self.region_id" },
);
__PACKAGE__->has_many(
  "nardis",
  "iPfamDB::Nardi",
  { "foreign.protein_region_id" => "self.region_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:xqbPOATAOcmsTe6bXZf32w


# You can replace this text with custom content, and it will be preserved on regeneration
1;
