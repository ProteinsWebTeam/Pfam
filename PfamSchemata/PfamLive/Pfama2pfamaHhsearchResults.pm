package PfamLive::Pfama2pfamaHhsearchResults;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA2pfamA_hhsearch_results");
__PACKAGE__->add_columns(
  "auto_pfama1",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "model_start1",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "model_end1",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "auto_pfama2",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "model_start2",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "model_end2",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "evalue",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 25 },
  "auto_pfama",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 5 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-08-25 16:13:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:lrdfDFGG+VyulCwC9oa2vw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
