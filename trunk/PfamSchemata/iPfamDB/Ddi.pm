package iPfamDB::Ddi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ddi");
__PACKAGE__->add_columns(
  "ddi",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "region_id_a",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_id_b",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "intrachain",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 3 },
);
__PACKAGE__->set_primary_key("ddi");
__PACKAGE__->belongs_to(
  "region_id_a",
  "iPfamDB::Domain",
  { region_id => "region_id_a" },
);
__PACKAGE__->belongs_to(
  "region_id_b",
  "iPfamDB::Domain",
  { region_id => "region_id_b" },
);
__PACKAGE__->has_many("ddi_res", "iPfamDB::DdiRes", { "foreign.ddi" => "self.ddi" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ldWxkKFyUdqSEKMDfFYw3A


# You can replace this text with custom content, and it will be preserved on regeneration
1;
