package RfamDB::GenomeSummary;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("genome_summary");
__PACKAGE__->add_columns(
  "ncbi_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "species",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "kingdom",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 50,
  },
  "regions",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "families",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "genome_size",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
);
__PACKAGE__->set_primary_key("ncbi_id");


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-11-28 14:26:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:DqzusYHLNl1KtXhfqdLI+g


# You can replace this text with custom content, and it will be preserved on regeneration
1;
