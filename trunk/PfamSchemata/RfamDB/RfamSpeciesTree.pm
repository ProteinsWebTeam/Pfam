package RfamDB::RfamSpeciesTree;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("rfam_species_tree");
__PACKAGE__->add_columns(
  "auto_rfam",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 5 },
  "json_string",
  {
    data_type => "MEDIUMTEXT",
    default_value => "",
    is_nullable => 0,
    size => 16777215,
  },
);
__PACKAGE__->belongs_to(
  "auto_rfam",
  "RfamDB::Rfam",
  { auto_rfam => "auto_rfam" },
);

__PACKAGE__->set_primary_key("auto_rfam");

1;
