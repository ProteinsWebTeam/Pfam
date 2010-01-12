package PfamLive::Taxonomy;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("taxonomy");
__PACKAGE__->add_columns(
  "ncbi_taxid",
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
__PACKAGE__->belongs_to(
  "ncbi_taxid",
  "PfamLive::NcbiTaxonomy",
  { ncbi_taxid => "ncbi_taxid" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:qsqa114ln2YRhFU0SMRhMQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
