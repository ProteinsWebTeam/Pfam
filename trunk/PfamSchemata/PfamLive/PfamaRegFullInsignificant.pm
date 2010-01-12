package PfamLive::PfamaRegFullInsignificant;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA_reg_full_insignificant");
__PACKAGE__->add_columns(
  "auto_pfama_reg_full",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 15 },
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "auto_pfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "seq_start",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "seq_end",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
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
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 15,
  },
  "sequence_bits_score",
  {
    data_type => "DOUBLE",
    default_value => "0.00",
    is_nullable => 0,
    size => 64,
  },
  "sequence_evalue_score",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 15,
  },
);
__PACKAGE__->set_primary_key("auto_pfama_reg_full");
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);
__PACKAGE__->belongs_to(
  "auto_pfamseq",
  "PfamLive::Pfamseq",
  { auto_pfamseq => "auto_pfamseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:m3/t0tpGNHietvOYnZCx0w


# You can replace this text with custom content, and it will be preserved on regeneration
1;
