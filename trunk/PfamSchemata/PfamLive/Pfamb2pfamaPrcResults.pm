package PfamLive::Pfamb2pfamaPrcResults;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamB2pfamA_PRC_results");
__PACKAGE__->add_columns(
  "auto_pfamb",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 6 },
  "model_start1",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "model_end1",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "length1",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "align1",
  {
    data_type => "BLOB",
    default_value => undef,
    is_nullable => 0,
    size => 65535,
  },
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "model_start2",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "model_end2",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "length2",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "align2",
  {
    data_type => "BLOB",
    default_value => undef,
    is_nullable => 0,
    size => 65535,
  },
  "evalue",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 25,
  },
);
__PACKAGE__->belongs_to(
  "auto_pfamb",
  "PfamLive::Pfamb",
  { auto_pfamb => "auto_pfamb" },
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:pi1nvCKh2b4xKztMADVOIQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
