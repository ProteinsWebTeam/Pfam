package PfamDB::Pdb;

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
  "author",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
);
__PACKAGE__->set_primary_key("pdb_id");
__PACKAGE__->has_many(
  "pdb_authors",
  "PfamDB::PdbAuthor",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "pdb_images",
  "PfamDB::PdbImage",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "pdb_pfama_regs",
  "PfamDB::PdbPfamaReg",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "pdb_pfamb_regs",
  "PfamDB::PdbPfambReg",
  { "foreign.pdb_id" => "self.pdb_id" },
);
__PACKAGE__->has_many(
  "pdb_residue_datas",
  "PfamDB::PdbResidueData",
  { "foreign.pdb_id" => "self.pdb_id" },
);

# this is redundant now. Manually modified to remove "pubmed_id" column
# and add "authors".
## Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-08-18 18:25:15
## DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:zuoHssnXw1lIyUTrfSBb/g


__PACKAGE__->might_have(
  "pdb_image",
  "PfamDB::PdbImage",
  { "foreign.pdb_id" => "self.pdb_id" },
);


1;
