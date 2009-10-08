package PfamDB::Pfamb2pfambPrcResults;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamB2pfamB_PRC_results");
__PACKAGE__->add_columns(
  "auto_pfamb1",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
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
  "auto_pfamb2",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
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


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:hsDvdUe3zGFaIWEPWknDMw


__PACKAGE__->has_one(
  "pfamB1",
  "PfamDB::Pfamb",
  { "foreign.auto_pfamb" => "self.auto_pfamb1" }
);

__PACKAGE__->has_one(
  "pfamB2",
  "PfamDB::Pfamb",
  { "foreign.auto_pfamb" => "self.auto_pfamb2" }
);


1;
