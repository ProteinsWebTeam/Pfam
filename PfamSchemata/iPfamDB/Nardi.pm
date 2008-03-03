package iPfamDB::Nardi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nardi");
__PACKAGE__->add_columns(
  "nardi",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "protein_region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "nucleic_region_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("nardi");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:furporUpOZUUMBKaCjaQCw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
