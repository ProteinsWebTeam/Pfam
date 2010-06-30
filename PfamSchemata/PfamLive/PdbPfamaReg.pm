package PfamLive::PdbPfamaReg;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_pfamA_reg");
__PACKAGE__->add_columns(
  "auto_pdb_reg",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 15 },
  "auto_pfama_reg_full",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 15 },
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_pfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "chain",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 4 },
  "pdb_res_start",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "pdb_start_icode",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 1 },
  "pdb_res_end",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "pdb_end_icode",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 1 },
  "seq_start",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "seq_end",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "hex_colour",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 6 },
);
__PACKAGE__->set_primary_key("auto_pdb_reg");
__PACKAGE__->belongs_to("pdb_id", "PfamLive::Pdb", { pdb_id => "pdb_id" });
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);
__PACKAGE__->belongs_to(
  "auto_pfama_reg_full",
  "PfamLive::PfamaRegFullSignificant",
  { auto_pfama_reg_full => "auto_pfama_reg_full" },
);
__PACKAGE__->belongs_to(
  "auto_pfamseq",
  "PfamLive::Pfamseq",
  { auto_pfamseq => "auto_pfamseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2010-06-18 16:34:13
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:z1Bd2ji/FPvaqAESsVH8Ug


# You can replace this text with custom content, and it will be preserved on regeneration
1;
