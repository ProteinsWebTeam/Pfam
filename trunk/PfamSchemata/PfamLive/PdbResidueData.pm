package PfamLive::PdbResidueData;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_residue_data");
__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "msd_chain",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 4 },
  "chain",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 4 },
  "serial",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "pdb_res",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "pdb_seq_number",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "pfamseq_acc",
  { data_type => "VARCHAR", default_value => 0, is_nullable => 0, size => 6 },
  "auto_pfamseq",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "seq_version",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 4 },
  "pfamseq_res",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "pfamseq_seq_number",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "dssp_code",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
);
__PACKAGE__->belongs_to("pdb_id", "PfamLive::Pdb", { pdb_id => "pdb_id" });


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-08-21 16:01:11
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:qUESoPEAVf9I1abhmoxGZg

__PACKAGE__->might_have( pfamA_reg_full_significant => "PfamLive::PfamaRegFullSignificant",
             { "foreign.auto_pfamseq" => "self.auto_pfamseq"});

__PACKAGE__->might_have( pfamA_reg_seed => "PfamLive::PfamaRegSeed",
             { "foreign.auto_pfamseq" => "self.auto_pfamseq"});

 __PACKAGE__->might_have( pfamB_reg => "PfamLive::PfambReg",
             { "foreign.auto_pfamseq" => "self.auto_pfamseq"});
# You can replace this text with custom content, and it will be preserved on regeneration
1;
