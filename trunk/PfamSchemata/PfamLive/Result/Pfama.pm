package PfamLive::Result::Pfama;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 5 },
  "pfama_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
  "pfama_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 16 },
  "previous_id",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "description",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 100 },
  "author",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "deposited_by",
  {
    data_type => "VARCHAR",
    default_value => "anon",
    is_nullable => 0,
    size => 100,
  },
  "seed_source",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "type",
  { data_type => "ENUM", default_value => "", is_nullable => 0, size => 6 },
  "comment",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "sequence_ga",
  { data_type => "DOUBLE", default_value => "", is_nullable => 0, size => 64 },
  "domain_ga",
  { data_type => "DOUBLE", default_value => "", is_nullable => 0, size => 64 },
  "sequence_tc",
  { data_type => "DOUBLE", default_value => "", is_nullable => 0, size => 64 },
  "domain_tc",
  { data_type => "DOUBLE", default_value => "", is_nullable => 0, size => 64 },
  "sequence_nc",
  { data_type => "DOUBLE", default_value => "", is_nullable => 0, size => 64 },
  "domain_nc",
  { data_type => "DOUBLE", default_value => "", is_nullable => 0, size => 64 },
  "buildmethod",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "model_length",
  { data_type => "MEDIUMINT", default_value => "", is_nullable => 0, size => 8 },
  "searchmethod",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "msv_lambda",
  { data_type => "DOUBLE", default_value => "", is_nullable => 0, size => 64 },
  "msv_mu",
  { data_type => "DOUBLE", default_value => "", is_nullable => 0, size => 64 },
  "viterbi_lambda",
  { data_type => "DOUBLE", default_value => "", is_nullable => 0, size => 64 },
  "viterbi_mu",
  { data_type => "DOUBLE", default_value => "", is_nullable => 0, size => 64 },
  "forward_lambda",
  { data_type => "DOUBLE", default_value => "", is_nullable => 0, size => 64 },
  "forward_tau",
  { data_type => "DOUBLE", default_value => "", is_nullable => 0, size => 64 },
  "num_seed",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "num_full",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "updated",
  {
    data_type => "TIMESTAMP",
    default_value => "CURRENT_TIMESTAMP",
    is_nullable => 0,
    size => 14,
  },
  "created",
  {
    data_type => "DATETIME",
    default_value => undef,
    is_nullable => 1,
    size => 19,
  },
  "version",
  {
    data_type => "SMALLINT",
    default_value => undef,
    is_nullable => 1,
    size => 5,
  },
  "number_archs",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "number_species",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "number_structures",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
"number_rp15",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "number_rp35",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "number_rp55",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "number_rp75",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },

  "number_ncbi",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "number_meta",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 8 },
  "average_length",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "percentage_id",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 3 },
  "average_coverage",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "change_status",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "seed_consensus",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "full_consensus",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "number_shuffled_hits",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
);
__PACKAGE__->set_primary_key("auto_pfama");
__PACKAGE__->add_unique_constraint("pfamA_id", ["pfama_id"]);
__PACKAGE__->add_unique_constraint("pfamA_acc", ["pfama_acc"]);
__PACKAGE__->has_many(
  "_active_site_alignments",
  "PfamLive::Result::ActiveSiteAlignments",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "_pfama_internals",
  "PfamLive::Result::PfamaInternal",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "alignments_and_trees",
  "PfamLive::Result::AlignmentsAndTrees",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "clan_memberships",
  "PfamLive::Result::ClanMembership",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "context_pfam_regions",
  "PfamLive::Result::ContextPfamRegions",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "current_pfam_versions",
  "PfamLive::Result::CurrentPfamVersion",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "edits",
  "PfamLive::Result::Edits",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "gene_ontologies",
  "PfamLive::Result::GeneOntology",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "interpros",
  "PfamLive::Result::Interpro",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "nested_domains",
  "PfamLive::Result::NestedDomains",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "nested_locations",
  "PfamLive::Result::NestedLocations",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pdb_pfama_regs",
  "PfamLive::Result::PdbPfamaReg",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pfama2pfama_hhsearch_results_auto_pfama2s",
  "PfamLive::Result::Pfama2pfamaHhsearchResults",
  { "foreign.auto_pfama2" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pfama2pfama_hhsearch_results_auto_pfama1s",
  "PfamLive::Result::Pfama2pfamaHhsearchResults",
  { "foreign.auto_pfama1" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pfama_hmms",
  "PfamLive::Result::PfamaHmm",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pfama_architectures",
  "PfamLive::Result::PfamaArchitecture",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pfama_database_links",
  "PfamLive::Result::PfamaDatabaseLinks",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pfama_fastas",
  "PfamLive::Result::PfamaFasta",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pfama_literature_references",
  "PfamLive::Result::PfamaLiteratureReferences",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pfama_ncbis",
  "PfamLive::Result::PfamaNcbi",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pfama_reg_full_insignificants",
  "PfamLive::Result::PfamaRegFullInsignificant",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pfama_reg_full_significants",
  "PfamLive::Result::PfamaRegFullSignificant",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pfama_reg_seeds",
  "PfamLive::Result::PfamaRegSeed",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pfama_species_trees",
  "PfamLive::Result::PfamaSpeciesTree",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pfama_tax_depths",
  "PfamLive::Result::PfamaTaxDepth",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "pfama_wikis",
  "PfamLive::Result::PfamaWiki",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "proteome_regions",
  "PfamLive::Result::ProteomeRegions",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "released_pfam_versions",
  "PfamLive::Result::ReleasedPfamVersion",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
__PACKAGE__->has_many(
  "_active_site_alignments",
  "PfamLive::Result::ActiveSiteAlignments",
  { "foreign.auto_pfama" => "self.auto_pfama" },
);
 
__PACKAGE__->many_to_many(
  'articles' => 'pfama_wikis', 'auto_wiki'
);
 
__PACKAGE__->set_primary_key("auto_pfama", "pfama_id", "pfama_acc");

__PACKAGE__->belongs_to(
  'from_dead',
  'PfamLive::Result::DeadFamilies',
  { 'foreign.forward_to' => 'self.pfamA_acc' }
);

1;
