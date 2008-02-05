package iPfamDB::Protein;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("protein");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 6 },
  "id",
  {
    data_type => "VARCHAR",
    default_value => "NULL",
    is_nullable => 1,
    size => 12,
  },
  "seq_version",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "md5",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 32 },
  "source_db",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "length",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "ncbi_code",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 11 },
  "sequence",
  { data_type => "BLOB", default_value => "", is_nullable => 0, size => 65535 },
);
__PACKAGE__->add_unique_constraint("proteins_accession_id_Idx", ["accession", "id"]);
__PACKAGE__->has_many(
  "domains",
  "iPfamDB::Domain",
  {
    "foreign.protein_accession" => "self.accession",
    "foreign.protein_id"        => "self.id",
  },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:bE532CtJWIC6QIT65VuCCA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
