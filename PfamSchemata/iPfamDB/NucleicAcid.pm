package iPfamDB::NucleicAcid;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nucleic_acid");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "seq_version",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 4 },
  "md5",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 32,
  },
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
__PACKAGE__->set_primary_key("accession");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:nN5Bcs5AZixz/iKEq5FFUg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
