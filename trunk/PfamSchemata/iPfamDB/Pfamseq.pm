package iPfamDB::Pfamseq;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamseq");
__PACKAGE__->add_columns(
  "auto_pfamseq",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "pfamseq_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
  "pfamseq_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 6 },
  "seq_version",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 4 },
  "crc64",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 16 },
  "md5",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 32 },
  "description",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "evidence",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 4 },
  "length",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "species",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "taxonomy",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "is_fragment",
  { data_type => "TINYINT", default_value => undef, is_nullable => 1, size => 1 },
  "sequence",
  { data_type => "BLOB", default_value => "", is_nullable => 0, size => 65535 },
  "updated",
  {
    data_type => "TIMESTAMP",
    default_value => "CURRENT_TIMESTAMP",
    is_nullable => 0,
    size => 14,
  },
  "created",
  {
    data_type => "DATETIME",
    default_value => undef,
    is_nullable => 1,
    size => 19,
  },
  "ncbi_taxid",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
  "genome_seq",
  { data_type => "TINYINT", default_value => 0, is_nullable => 1, size => 1 },
  "auto_architecture",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "treefam_acc",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 8 },
);
__PACKAGE__->set_primary_key("auto_pfamseq");
__PACKAGE__->add_unique_constraint("pfamseq_acc", ["pfamseq_acc"]);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:AtGvESgQEgTWRTaV2xAwDA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
