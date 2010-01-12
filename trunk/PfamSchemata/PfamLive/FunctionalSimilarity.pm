package PfamLive::FunctionalSimilarity;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("functional_similarity");
__PACKAGE__->add_columns(
  "auto_pfama_a",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_pfama_b",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "rfunsim",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "mfscore",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "bpscore",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:i4JVvRESIFggxDSrB6PAgQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
