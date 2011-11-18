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
  { auto_pfamb => "auto_pfamb" },
);

__PACKAGE__->set_primary_key("auto_pfamb");

1;
