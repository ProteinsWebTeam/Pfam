package PfamLive::PfamaNcbi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA_ncbi");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 5 },
  "pfama_acc",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 0, size => 7 },
  "pfama_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 40,
  },
  "ncbi_taxid",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);
__PACKAGE__->belongs_to(
  "ncbi_taxid",
  "PfamLive::NcbiTaxonomy",
  { ncbi_taxid => "ncbi_taxid" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:wF44Yfzhit+wLszAfDYOMA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
