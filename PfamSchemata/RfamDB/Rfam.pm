package RfamDB::Rfam;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("rfam");
__PACKAGE__->add_columns(
  "auto_rfam",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "rfam_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
  "rfam_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 40 },
  "description",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "author",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "seed_source",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "alignment_method",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "gathering_cutoff",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "trusted_cutoff",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "noise_cutoff",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "comment",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "previous_id",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "cmbuild",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "cmcalibrate",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "num_seed",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
  "num_full",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
  "type",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 50,
  },
  "structure_source",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "number_of_states",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "number_of_nodes",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "number_of_species",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
  "taxonomic_domain",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "taxonomic_root",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
);
__PACKAGE__->set_primary_key("auto_rfam");
__PACKAGE__->add_unique_constraint("rfam_acc", ["rfam_acc"]);
__PACKAGE__->has_many(
  "alignments_and_trees",
  "RfamDB::AlignmentsAndTrees",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "html_alignments",
  "RfamDB::HtmlAlignments",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "rfam_cms",
  "RfamDB::RfamCm",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "rfam_database_links",
  "RfamDB::RfamDatabaseLinks",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "rfam_literature_references",
  "RfamDB::RfamLiteratureReferences",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "rfam_reg_fulls",
  "RfamDB::RfamRegFull",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "rfam_reg_seeds",
  "RfamDB::RfamRegSeed",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "secondary_structure_images",
  "RfamDB::SecondaryStructureImages",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "wikis",
  "RfamDB::Wiki",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-14 20:19:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:02dlF/JEau6RmLwYF9l7gA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
