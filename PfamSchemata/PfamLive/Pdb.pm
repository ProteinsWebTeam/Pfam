package PfamLive::Pdb;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb");
__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "keywords",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "title",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "date",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "resolution",
  {
    data_type => "DECIMAL",
    default_value => "0.00",
    is_nullable => 1,
    size => 5,
  },
  "method",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  #"pubmed_id",
  #{ data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
);
__PACKAGE__->set_primary_key("pdb_id");
__PACKAGE__->has_many(
  "pdb_authors",
  "PfamLive::PdbAuthor",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "pdb_images",
  "PfamLive::PdbImage",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "pdb_pfama_regs",
  "PfamLive::PdbPfamaReg",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "pdb_pfamb_regs",
  "PfamLive::PdbPfambReg",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "pdb_residue_datas",
  "PfamLive::PdbResidueData",
  { "foreign.pdb_id" => "self.pdb_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-08-18 18:25:15
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:zuoHssnXw1lIyUTrfSBb/g


# You can replace this text with custom content, and it will be preserved on regeneration
1;
