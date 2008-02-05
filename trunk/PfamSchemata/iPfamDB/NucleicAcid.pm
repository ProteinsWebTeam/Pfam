package iPfamDB::NucleicAcid;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nucleic_acid");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 6 },
  "id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
  "seq_version",
  { data_type => "BIGINT", default_value => "", is_nullable => 0, size => 20 },
  "type",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 3 },
  "source_db",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "length",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "ncbi_code",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "sequence",
  { data_type => "BLOB", default_value => "", is_nullable => 0, size => 65535 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:+h0ieDbz7W+7l2cF4tHR2Q


# You can replace this text with custom content, and it will be preserved on regeneration
1;
