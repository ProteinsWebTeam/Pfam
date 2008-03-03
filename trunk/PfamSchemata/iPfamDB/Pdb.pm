package iPfamDB::Pdb;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "pdb_id",
  {
    data_type => "VARCHAR",
    default_value => "NULL",
    is_nullable => 0,
    size => 5,
  },
  "header",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "title",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "date",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "resolution",
  { data_type => "DECIMAL", default_value => "", is_nullable => 0, size => 5 },
  "experiment_short",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "experiment_long",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "pubmed_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "biological_unit",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 4 },
);
__PACKAGE__->set_primary_key("accession");
__PACKAGE__->add_unique_constraint("pdb_accession_Idx", ["pdb_id", "biological_unit"]);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Fadvli9zZizARcjeQnxobQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
