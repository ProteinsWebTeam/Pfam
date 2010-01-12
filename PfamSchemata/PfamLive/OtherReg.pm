package PfamLive::OtherReg;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("other_reg");
__PACKAGE__->add_columns(
  "region_id",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "auto_pfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "seq_start",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "seq_end",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 6 },
  "type_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 20,
  },
  "source_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 20,
  },
  "score",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "orientation",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 4 },
);
__PACKAGE__->set_primary_key("region_id");
__PACKAGE__->belongs_to(
  "auto_pfamseq",
  "PfamLive::Pfamseq",
  { auto_pfamseq => "auto_pfamseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:fPIRkbR1wB8SDhxzllHxCA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
