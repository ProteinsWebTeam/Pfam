package PfamLive::PfambSpeciesTree;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamB_species_tree");
__PACKAGE__->add_columns(
  "auto_pfamb",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 5 },
  "json_string",
  {
    data_type => "LONGTEXT",
    default_value => "",
    is_nullable => 0,
    size => 16777215,
  },
);
__PACKAGE__->belongs_to(
  "auto_pfamb",
  "PfamLive::Pfamb",
  { auto_pfama => "auto_pfamb" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2010-06-18 14:21:30
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:XfajFCpxLLWe3IruJUuMfA
__PACKAGE__->set_primary_key("auto_pfamb");

# You can replace this text with custom content, and it will be preserved on regeneration
1;
