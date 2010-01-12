package PfamLive::NcbiSeq;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ncbi_seq");
__PACKAGE__->add_columns(
  "gi",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "secondary_acc",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 12,
  },
  "tertiary_acc",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 23,
  },
  "md5",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 32,
  },
  "description",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 0,
    size => 65535,
  },
  "length",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "sequence",
  {
    data_type => "BLOB",
    default_value => undef,
    is_nullable => 0,
    size => 65535,
  },
);
__PACKAGE__->set_primary_key("gi");
__PACKAGE__->has_many(
  "metaseq_ncbi_maps",
  "PfamLive::MetaseqNcbiMap",
  { "foreign.gi" => "self.gi" },
);
__PACKAGE__->has_many(
  "ncbi_maps",
  "PfamLive::NcbiMap",
  { "foreign.gi" => "self.gi" },
);
__PACKAGE__->has_many(
  "ncbi_pfama_regs",
  "PfamLive::NcbiPfamaReg",
  { "foreign.gi" => "self.gi" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:eX5ZlLLYr7gG802E8bTZJg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
