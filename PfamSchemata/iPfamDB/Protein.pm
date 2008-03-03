package iPfamDB::Protein;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("protein");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
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
__PACKAGE__->set_primary_key("accession");
__PACKAGE__->add_unique_constraint("proteins_accession_id_Idx", ["accession", "id"]);
__PACKAGE__->add_unique_constraint("proteins_accession_Idx", ["accession"]);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:EkB15hNDw3aoIz43wP43KQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
