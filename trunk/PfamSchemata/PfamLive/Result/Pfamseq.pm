package PfamLive::Result::Pfamseq;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components(qw/Core Result::ColumnData/);

__PACKAGE__->table("pfamseq");
__PACKAGE__->add_columns(
  "auto_pfamseq",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "pfamseq_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
  "pfamseq_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 6 },
  "seq_version",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 4 },
  "crc64",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 16 },
  "md5",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 32 },
  "description",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "evidence",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 4 },
  "length",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "species",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "taxonomy",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "is_fragment",
  { data_type => "TINYINT", default_value => undef, is_nullable => 1, size => 1 },
  "sequence",
  { data_type => "BLOB", default_value => "", is_nullable => 0, size => 65535 },
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
  "ncbi_taxid",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
  "genome_seq",
  { data_type => "TINYINT", default_value => 0, is_nullable => 1, size => 1 },
  "auto_architecture",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "treefam_acc",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 8 },
  "rp15",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 1 },
  "rp35",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 1 },
  "rp55",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 1 },
  "rp75",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 1 },
  
);
__PACKAGE__->set_primary_key("auto_pfamseq");
__PACKAGE__->add_unique_constraint("pfamseq_acc", ["pfamseq_acc"]);
__PACKAGE__->has_many(
  "context_pfam_regions",
  "PfamLive::Result::ContextPfamRegions",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "edits",
  "PfamLive::Result::Edits",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "genome_pfamseqs",
  "PfamLive::Result::GenomePfamseq",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "ncbi_maps",
  "PfamLive::Result::NcbiMap",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "nested_locations",
  "PfamLive::Result::NestedLocations",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "other_regs",
  "PfamLive::Result::OtherReg",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pdb_pfama_regs",
  "PfamLive::Result::PdbPfamaReg",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pdb_pfamb_regs",
  "PfamLive::Result::PdbPfambReg",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pdb_residue_datas",
  "PfamLive::Result::PdbResidueData",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pfama_reg_full_insignificants",
  "PfamLive::Result::PfamaRegFullInsignificant",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pfama_reg_full_significants",
  "PfamLive::Result::PfamaRegFullSignificant",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pfama_reg_seeds",
  "PfamLive::Result::PfamaRegSeed",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pfamb_regs",
  "PfamLive::Result::PfambReg",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pfam_annseqs",
  "PfamLive::Result::PfamAnnseq",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->belongs_to(
  "ncbi_taxid",
  "PfamLive::Result::NcbiTaxonomy",
  { ncbi_taxid => "ncbi_taxid" },
);
__PACKAGE__->has_many(
  "pfamseq_disulphides",
  "PfamLive::Result::PfamseqDisulphide",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pfamseq_ncbis",
  "PfamLive::Result::PfamseqNcbi",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "proteome_regions",
  "PfamLive::Result::ProteomeRegions",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "secondary_pfamseq_accs",
  "PfamLive::Result::SecondaryPfamseqAcc",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2009-08-11 15:25:08
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:UWrKamczCwlDaIyS7X17Gg

__PACKAGE__->has_many(
  "pfamseq_markups",
  "PfamLive::Result::PfamseqMarkup",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);

 
__PACKAGE__->register_relationships_column_data();

# You can replace this text with custom content, and it will be preserved on regeneration
1;
