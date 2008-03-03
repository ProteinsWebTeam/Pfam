package iPfamDB::PdbChainData;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_chain_data");
__PACKAGE__->add_columns(
  "internal_chain_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "internal_chain_accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "original_chain",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 1 },
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
);
__PACKAGE__->add_unique_constraint("internal_chain_accession_idx", ["internal_chain_accession"]);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:OXB3Iz/BYc6GlHBOqUjO/Q


# You can replace this text with custom content, and it will be preserved on regeneration

1;
