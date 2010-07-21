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
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "crc64",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 16 },
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
__PACKAGE__->add_unique_constraint("proteins_accession_id_Idx", ["accession", "id"]);
__PACKAGE__->has_many(
  "pdb_chain_datas",
  "iPfamDB::PdbChainData",
  { "foreign.internal_chain_accession" => "self.accession" },
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
__PACKAGE__->belongs_to("source_db", "iPfamDB::Sources", { source_db => "source_db" });
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


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 15:16:33
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:gJYUdIFWHsjA12n7WIVmyg
# These lines were loaded from '/software/pfam/Modules/PfamSchemata/iPfamDB/Protein.pm' found in @INC.# They are now part of the custom portion of this file# for you to hand-edit.  If you do not either delete# this section or remove that file from @INC, this section# will be repeated redundantly when you re-create this# file again via Loader!
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
# End of lines loaded from '/software/pfam/Modules/PfamSchemata/iPfamDB/Protein.pm' 


# You can replace this text with custom content, and it will be preserved on regeneration
1;
