package RfamDB::TaxonomyWebsearch;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("taxonomy_websearch");
__PACKAGE__->add_columns(
  "ncbi_code",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
  "species",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "taxonomy",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "lft",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "rgt",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "parent",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "level",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-09-11 09:48:06
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:gfAIOvMannjuk9xJmFRuHQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
