package RfamDB::Rfamseq;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("rfamseq");
__PACKAGE__->add_columns(
  "auto_rfamseq",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "rfamseq_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
  "rfamseq_acc",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 50,
  },
  "description",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 255 },
  "species",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 100 },
  "taxonomy",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "version",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 12,
  },
  "previous_acc",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "taxon",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "auto_taxid",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("auto_rfamseq");
__PACKAGE__->add_unique_constraint("rfamseq_id", ["rfamseq_id"]);
__PACKAGE__->has_many(
  "chromosome_builds",
  "RfamDB::ChromosomeBuild",
  { "foreign.auto_rfamseq" => "self.auto_rfamseq" },
);
__PACKAGE__->has_many(
  "rfam_reg_fulls",
  "RfamDB::RfamRegFull",
  { "foreign.auto_rfamseq" => "self.auto_rfamseq" },
);
__PACKAGE__->has_many(
  "rfam_reg_seeds",
  "RfamDB::RfamRegSeed",
  { "foreign.auto_rfamseq" => "self.auto_rfamseq" },
);
__PACKAGE__->belongs_to(
  "auto_taxid",
  "RfamDB::Taxonomy",
  { auto_taxid => "auto_taxid" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-14 20:19:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:5g10c7qpEIPwx+5AxCKyqA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
