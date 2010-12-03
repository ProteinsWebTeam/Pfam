package PfamLive::NcbiTaxonomy;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ncbi_taxonomy");
__PACKAGE__->add_columns(
  "ncbi_taxid",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "species",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 100,
  },
  "taxonomy",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 0,
    size => 16777215,
  },
);
__PACKAGE__->set_primary_key("ncbi_taxid");
#__PACKAGE__->has_many(
#  "complete_proteomes",
#  "PfamLive::CompleteProteomes",
#  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
#);
__PACKAGE__->has_many(
  "pfama_ncbis",
  "PfamLive::PfamaNcbi",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
);
__PACKAGE__->has_many(
  "pfamseqs",
  "PfamLive::Pfamseq",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
);
__PACKAGE__->has_many(
  "pfamseq_ncbis",
  "PfamLive::PfamseqNcbi",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
);
__PACKAGE__->has_many(
  "taxonomies",
  "PfamLive::Taxonomy",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:MoDHs5Xa0ROZJmOAUoay3w

# You can replace this text with custom content, and it will be preserved on regeneration
1;
