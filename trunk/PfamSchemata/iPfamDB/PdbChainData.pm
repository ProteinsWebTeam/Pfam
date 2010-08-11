package iPfamDB::PdbChainData;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_chain_data");
__PACKAGE__->add_columns(
  "internal_chain_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "internal_chain_accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "original_chain",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 1 },
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
);
__PACKAGE__->add_unique_constraint("internal_chain_accession_idx", ["internal_chain_accession"]);
__PACKAGE__->add_unique_constraint("UQ_pdb_chain_data_1", ["internal_chain_accession"]);
__PACKAGE__->has_many(
  "ligands",
  "iPfamDB::Ligands",
  { "foreign.accession" => "self.internal_chain_accession" },
);
__PACKAGE__->has_many(
  "nucleic_acids",
  "iPfamDB::NucleicAcid",
  { "foreign.accession" => "self.internal_chain_accession" },
);
__PACKAGE__->belongs_to(
  "internal_chain_accession",
  "iPfamDB::Protein",
  { accession => "internal_chain_accession" },
);
__PACKAGE__->belongs_to("pdb_id", "iPfamDB::Pdb", { pdb_id => "pdb_id" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:2yDOxYWMRSoAEfqIudiQKg



# You can replace this text with custom content, and it will be preserved on regeneration
1;
