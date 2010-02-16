package PfamDB::Pfamseq;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
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
);
__PACKAGE__->set_primary_key("auto_pfamseq");
__PACKAGE__->add_unique_constraint("pfamseq_acc", ["pfamseq_acc"]);
__PACKAGE__->has_many(
  "context_pfam_regions",
  "PfamDB::ContextPfamRegions",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "edits",
  "PfamDB::Edits",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "genome_pfamseqs",
  "PfamDB::GenomePfamseq",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);


__PACKAGE__->has_many(
  "nested_locations",
  "PfamDB::NestedLocations",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "other_regs",
  "PfamDB::OtherReg",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pdb_pfama_regs",
  "PfamDB::PdbPfamaReg",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pdb_pfamb_regs",
  "PfamDB::PdbPfambReg",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pdb_residue_datas",
  "PfamDB::PdbResidueData",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pfama_reg_full_insignificants",
  "PfamDB::PfamaRegFullInsignificant",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pfama_reg_full_significants",
  "PfamDB::PfamaRegFullSignificant",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pfama_reg_seeds",
  "PfamDB::PfamaRegSeed",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pfamb_regs",
  "PfamDB::PfambReg",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pfam_annseqs",
  "PfamDB::PfamAnnseq",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->belongs_to(
  "ncbi_taxid",
  "PfamDB::NcbiTaxonomy",
  { ncbi_taxid => "ncbi_taxid" },
);
__PACKAGE__->has_many(
  "pfamseq_disulphides",
  "PfamDB::PfamseqDisulphide",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "pfamseq_ncbis",
  "PfamDB::PfamseqNcbi",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "proteome_regions",
  "PfamDB::ProteomeRegions",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);
__PACKAGE__->has_many(
  "secondary_pfamseq_accs",
  "PfamDB::SecondaryPfamseqAcc",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2009-08-11 15:25:08
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:UWrKamczCwlDaIyS7X17Gg


__PACKAGE__->has_many(
  "pfamseq_markups",
  "PfamDB::PfamseqMarkup",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);

__PACKAGE__->has_one(
  "annseqs",
  "PfamDB::PfamAnnseq",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);

__PACKAGE__->has_many(
  "proteome_pfamseqs",
  "PfamDB::ProteomePfamseq",
  { "foreign.auto_pfamseq" => "self.auto_pfamseq" },
);

# this relationship needs to have the same name as the one that links 
# PfamaArchitecture to Architecture
__PACKAGE__->belongs_to(
  "auto_architecture",
  "PfamDB::Architecture",
  { "foreign.auto_architecture" => "self.auto_architecture" },
);


=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut


1;
