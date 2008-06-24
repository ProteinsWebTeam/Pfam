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
  "html",
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
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "treemethod",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
);
__PACKAGE__->belongs_to("auto_rfam", "RfamDB::Rfam", { auto_rfam => "auto_rfam" });


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-06-06 20:11:27
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:mmmmzqR5DEFAJ9px5u4TEA
__PACKAGE__->add_unique_constraint( 'ali_constraint' => ["auto_rfam",'type']);
__PACKAGE__->set_primary_key('auto_rfam','type');


# You can replace this text with custom content, and it will be preserved on regeneration
1;
