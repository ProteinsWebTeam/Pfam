package RfamDB::Taxonomy;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("taxonomy");
__PACKAGE__->add_columns(
  "auto_taxid",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "ncbi_id",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "species",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 100 },
  "tax_string",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
);
__PACKAGE__->set_primary_key("auto_taxid");
__PACKAGE__->add_unique_constraint("ncbi_id", ["ncbi_id"]);
__PACKAGE__->has_many(
  "rfamseqs",
  "RfamDB::Rfamseq",
  { "foreign.auto_taxid" => "self.auto_taxid" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-14 20:19:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:iKaadra7dPWKEr/zxHo3Ow


# You can replace this text with custom content, and it will be preserved on regeneration
1;
