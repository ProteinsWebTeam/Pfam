package PfamLive::MetaPfamaReg;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("meta_pfamA_reg");
__PACKAGE__->add_columns(
  "auto_meta_pfama_reg",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 15 },
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "auto_metaseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "seq_start",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "seq_end",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "ali_start",
  { data_type => "MEDIUMINT", default_value => "", is_nullable => 0, size => 8 },
  "ali_end",
  { data_type => "MEDIUMINT", default_value => "", is_nullable => 0, size => 8 },
  "model_start",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "model_end",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "domain_bits_score",
  {
    data_type => "DOUBLE",
    default_value => "0.00",
    is_nullable => 0,
    size => 64,
  },
  "domain_evalue_score",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 15 },
  "sequence_bits_score",
  {
    data_type => "DOUBLE",
    default_value => "0.00",
    is_nullable => 0,
    size => 64,
  },
  "sequence_evalue_score",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 15 },
  "cigar",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "in_full",
  { data_type => "TINYINT", default_value => 0, is_nullable => 0, size => 1 },
  "tree_order",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 9,
  },
);
__PACKAGE__->set_primary_key("auto_meta_pfama_reg");
__PACKAGE__->belongs_to(
  "auto_metaseq",
  "PfamLive::Metaseq",
  { auto_metaseq => "auto_metaseq" },
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-07-23 10:58:05
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:+741caBYokdxF6G0KA8iHw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
