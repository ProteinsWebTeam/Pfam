package iPfamDB::Ddi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ddi");
__PACKAGE__->add_columns(
  "ddi",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "region_id_a",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "region_id_b",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "intrachain",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 1 },
);
__PACKAGE__->set_primary_key("ddi");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:f/YtXUE76gUbAg81Ekofng


# You can replace this text with custom content, and it will be preserved on regeneration
__PACKAGE__->add_unique_constraint("ddiConst", ["region_id_a", "region_id_b", "intrachain"]);
1;
