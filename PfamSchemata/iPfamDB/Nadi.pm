package iPfamDB::Nadi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nadi");
__PACKAGE__->add_columns(
  "nadi",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "nucleic_acid_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("nadi");
__PACKAGE__->belongs_to(
  "nucleic_acid_acc",
  "iPfamDB::NucleicAcid",
  { accession => "nucleic_acid_acc" },
);
__PACKAGE__->belongs_to("region_id", "iPfamDB::Domain", { region_id => "region_id" });
__PACKAGE__->has_many(
  "nadi_res",
  "iPfamDB::NadiRes",
  { "foreign.nadi" => "self.nadi" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:i8JrYEgEE7XgauSED/eOMg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
