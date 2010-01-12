package PfamLive::Metaseq;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("metaseq");
__PACKAGE__->add_columns(
  "auto_metaseq",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "metaseq_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 30,
  },
  "metaseq_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 30 },
  "description",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "length",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "sequence",
  { data_type => "BLOB", default_value => "", is_nullable => 0, size => 65535 },
  "source",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "metagenomics",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 1 },
  "md5",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 32,
  },
);
__PACKAGE__->set_primary_key("auto_metaseq");
__PACKAGE__->has_many(
  "meta_pfama_regs",
  "PfamLive::MetaPfamaReg",
  { "foreign.auto_metaseq" => "self.auto_metaseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2009-07-29 12:58:50
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:WCQhAt5wCKzRGsZ7ROOzvA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
