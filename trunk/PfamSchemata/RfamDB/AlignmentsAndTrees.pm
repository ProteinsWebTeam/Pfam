package RfamDB::AlignmentsAndTrees;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("alignments_and_trees");
__PACKAGE__->add_columns(
  "auto_rfam",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "alignment",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "tree",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "type",
  { data_type => "ENUM", default_value => "", is_nullable => 0, size => 4 },
  "treemethod",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "average_length",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "percent_id",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "number_of_sequences",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
  "most_unrelated_pair",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
);
__PACKAGE__->set_primary_key("auto_rfam", "type");


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-08 22:27:02
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:CqoA8eaiPaap7ZP608OJPw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
