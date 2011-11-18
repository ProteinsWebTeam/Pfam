package PfamLive::Pfamb;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamB");
__PACKAGE__->add_columns(
  "auto_pfamb",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 6 },
  "pfamb_acc",
  { data_type => "CHAR", default_value => undef, is_nullable => 0, size => 8 },
  "pfamb_id",
  { data_type => "CHAR", default_value => undef, is_nullable => 0, size => 15 },
  "number_archs",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "number_regions",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
  "number_species",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "number_structures",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
);
__PACKAGE__->set_primary_key("auto_pfamb");
__PACKAGE__->has_many(
  "pdb_pfamb_regs",
  "PfamLive::PdbPfambReg",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
);
__PACKAGE__->has_many(
  "pfamb2pfama_prc_results",
  "PfamLive::Pfamb2pfamaPrcResults",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
);
__PACKAGE__->has_many(
  "pfamb_database_links",
  "PfamLive::PfambDatabaseLinks",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
);
__PACKAGE__->has_many(
  "pfamb_fastas",
  "PfamLive::PfambFasta",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
);
__PACKAGE__->has_many(
  "pfamb_regs",
  "PfamLive::PfambReg",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
);
__PACKAGE__->has_many(
  "pfamb_stockholms",
  "PfamLive::PfambStockholm",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:66AT/7dRzxlUUAGTsmogwg

__PACKAGE__->has_many(
  "pfamb_species_trees",
  "PfamLive::PfambSpeciesTree",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
);

# You can replace this text with custom content, and it will be preserved on regeneration
1;
