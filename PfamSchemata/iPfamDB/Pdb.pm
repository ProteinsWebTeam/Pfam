package iPfamDB::Pdb;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb");
__PACKAGE__->add_columns(
  "pdb_id",
  {
    data_type => "VARCHAR",
    default_value => "NULL",
    is_nullable => 0,
    size => 5,
  },
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
  { data_type => "DECIMAL", default_value => "", is_nullable => 0, size => 5 },
  "method",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "pubmed_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("pdb_id");
__PACKAGE__->add_unique_constraint("UQ_pdb_1", ["pdb_id"]);
__PACKAGE__->has_many(
  "pdb_chain_datas",
  "iPfamDB::PdbChainData",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "pdb_images",
  "iPfamDB::PdbImage",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "pdb_residue_datas",
  "iPfamDB::PdbResidueData",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "trackings",
  "iPfamDB::Tracking",
  { "foreign.pdb_id" => "self.pdb_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:UZeFLr/iWUS9N7R740u+wA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
