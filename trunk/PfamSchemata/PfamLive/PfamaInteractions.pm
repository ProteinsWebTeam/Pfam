package PfamLive::PfamaInteractions;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA_interactions");
__PACKAGE__->add_columns(
  "auto_pfama_a",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "auto_pfama_b",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:do6T35QdwsMj5EIok8v9kw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
