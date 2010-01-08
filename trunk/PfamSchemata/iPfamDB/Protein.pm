package iPfamDB::Protein;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("protein");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "seq_version",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "md5",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 32 },
  "source_db",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "length",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "ncbi_code",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "sequence",
  { data_type => "BLOB", default_value => "", is_nullable => 0, size => 65535 },
);
__PACKAGE__->set_primary_key("accession");
__PACKAGE__->has_many(
  "domains",
  "iPfamDB::Domain",
  { "foreign.protein_accession" => "self.accession" },
);
__PACKAGE__->has_many(
  "plis",
  "iPfamDB::Pli",
  { "foreign.protein_acc" => "self.accession" },
);
__PACKAGE__->has_many(
  "pli_res",
  "iPfamDB::PliRes",
  { "foreign.protein_acc" => "self.accession" },
);
__PACKAGE__->has_many(
  "ppi_protein_acc_as",
  "iPfamDB::Ppi",
  { "foreign.protein_acc_a" => "self.accession" },
);
__PACKAGE__->has_many(
  "ppi_protein_acc_bs",
  "iPfamDB::Ppi",
  { "foreign.protein_acc_b" => "self.accession" },
);
__PACKAGE__->belongs_to(
  "accession",
  "iPfamDB::PdbChainData",
  { "internal_chain_accession" => "accession" },
);
__PACKAGE__->has_many(
  "protein_annseqs",
  "iPfamDB::ProteinAnnseq",
  { "foreign.accession" => "self.accession" },
);
__PACKAGE__->has_many(
  "protein_int_atoms",
  "iPfamDB::ProteinIntAtoms",
  { "foreign.protein_acc" => "self.accession" },
);


# Created by DBIx::Class::Schema::Loader v0.04006 @ 2010-01-08 13:31:47
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:/6FpTg5X20qLw5GetANjoA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
