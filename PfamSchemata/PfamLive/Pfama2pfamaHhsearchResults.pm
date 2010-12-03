package PfamLive::Pfama2pfamaHhsearchResults;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA2pfamA_hhsearch_results");
__PACKAGE__->add_columns(
  "auto_pfama1",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "auto_pfama2",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "evalue",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 25 },
);
__PACKAGE__->belongs_to(
  "auto_pfama2",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama2" },
);
__PACKAGE__->belongs_to(
  "auto_pfama1",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama1" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-11-16 14:52:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:aPCgnutLXO6dtpfU4WSgfQ
__PACKAGE__->add_unique_constraint("pairwise", [qw(auto_pfama1 auto_pfama2)]);
__PACKAGE__->set_primary_key("auto_pfama1", "auto_pfama2");
# You can replace this text with custom content, and it will be preserved on regeneration
1;
