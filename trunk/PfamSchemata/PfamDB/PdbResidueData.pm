package PfamDB::PdbResidueData;

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
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "seq_version",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 4 },
  "pfamseq_res",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "pfamseq_seq_number",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "dssp_code",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
);
__PACKAGE__->belongs_to("pdb_id", "PfamDB::Pdb", { pdb_id => "pdb_id" });


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-08-18 18:25:15
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:tq5FVjWI0/xlqx6guuwfSA


__PACKAGE__->might_have(
  "pfamseqs",
  "PfamDB::Pfamseq",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
);

__PACKAGE__->might_have(
  'pfamseqMarkup',
  'PfamDB::Pfamseq_markup',
  { "foreign.auto_pfamseq" => "self.auto_pfamseq",
    "foreign.residue"      => "self.pfamseq_seq_number" },
    { proxy => [ qw/ annotation auto_markup / ] } );

#__PACKAGE__->might_have( pfamA_reg_full_significant => "PfamDB::PfamaRegFullSignificant",
#             { "foreign.auto_pfamseq" => "self.auto_pfamseq"});

#__PACKAGE__->might_have( pfamA_reg_seed => "PfamDB::PfamaRegSeed",
#             { "foreign.auto_pfamseq" => "self.auto_pfamseq"});


1;
