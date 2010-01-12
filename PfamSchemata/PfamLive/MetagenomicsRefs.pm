package PfamLive::MetagenomicsRefs;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("metagenomics_refs");
__PACKAGE__->add_columns(
  "auto_meta_ref",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "source",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 0,
    size => 255,
  },
  "long_source",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "pmid",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("auto_meta_ref");


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:4KdWNtgZMvlbXDQY4Za4cg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
