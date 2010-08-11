package iPfamDB::NadiRes;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nadi_res");
__PACKAGE__->add_columns(
  "nadi",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "nucleic_acid_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "base",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_residue",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "bond",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
);
#__PACKAGE__->set_primary_key("nadi");
__PACKAGE__->belongs_to("nadi", "iPfamDB::Nadi", { nadi => "nadi" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:J+C6+BXdaMakW+06ET6Mng

# I have commented the set_primary_key line above the check_sum, so remember to move this file when new dump is created;

# You can replace this text with custom content, and it will be preserved on regeneration
1;
