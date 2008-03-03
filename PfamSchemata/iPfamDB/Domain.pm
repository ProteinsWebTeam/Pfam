package iPfamDB::Domain;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("domain");
__PACKAGE__->add_columns(
  "pfam_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 8 },
  "protein_accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "start",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "end",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "region_id",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "region_source_db",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
  "protein_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
);
__PACKAGE__->set_primary_key("region_id");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:xOcxNlDEtrSVi2I/9L4ISg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
