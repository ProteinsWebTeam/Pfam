package PfamLive::SeqInfo;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("seq_info");
__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 0, size => 7 },
  "pfama_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 40,
  },
  "description",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_pfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "pfamseq_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 12,
  },
  "pfamseq_acc",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 0, size => 6 },
  "seq_description",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 0,
    size => 65535,
  },
  "species",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 0,
    size => 65535,
  },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:5qPZWTcR/pB5butXe31hew


# You can replace this text with custom content, and it will be preserved on regeneration
1;
