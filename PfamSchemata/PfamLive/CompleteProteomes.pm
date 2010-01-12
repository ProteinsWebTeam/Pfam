package PfamLive::CompleteProteomes;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("complete_proteomes");
__PACKAGE__->add_columns(
  "auto_proteome",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "ncbi_taxid",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "species",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "grouping",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "num_distinct_regions",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "num_total_regions",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "num_proteins",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "sequence_coverage",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 3 },
  "residue_coverage",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 3 },
  "total_genome_proteins",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "total_aa_length",
  { data_type => "BIGINT", default_value => 0, is_nullable => 0, size => 20 },
  "total_aa_covered",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
  "total_seqs_covered",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
);
__PACKAGE__->set_primary_key("auto_proteome");
__PACKAGE__->add_unique_constraint("ncbi_taxid", ["ncbi_taxid"]);
__PACKAGE__->has_many(
  "genome_pfamseqs",
  "PfamLive::GenomePfamseq",
  { "foreign.auto_proteome" => "self.auto_proteome" },
);
__PACKAGE__->has_many(
  "proteome_regions",
  "PfamLive::ProteomeRegions",
  { "foreign.auto_proteome" => "self.auto_proteome" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-08-20 12:44:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ynOLI+xaLIb64KE+/gYyig


# You can replace this text with custom content, and it will be preserved on regeneration
1;
