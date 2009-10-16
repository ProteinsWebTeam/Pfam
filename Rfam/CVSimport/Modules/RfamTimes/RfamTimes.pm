package RfamTimes::RfamTimes;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("rfam_times");
__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 40 },
  "calibration",
  { data_type => "BIGINT", default_value => 0, is_nullable => 1, size => 20 },
  "blast",
  { data_type => "BIGINT", default_value => 0, is_nullable => 1, size => 20 },
  "cmsearch",
  { data_type => "BIGINT", default_value => 0, is_nullable => 1, size => 20 },
  "cmalign",
  { data_type => "BIGINT", default_value => 0, is_nullable => 1, size => 20 },
  "rfsearchwall",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
);
__PACKAGE__->set_primary_key("rfam_acc");


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2009-08-23 21:54:36
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:4HbCfMTXqUCocY6K8pZMsQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
