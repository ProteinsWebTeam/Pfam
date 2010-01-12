package PfamLive::PfambReg;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamB_reg");
__PACKAGE__->add_columns(
  "auto_pfamb_reg",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 15 },
  "auto_pfamb",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 6 },
  "auto_pfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "seq_start",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "seq_end",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
);
__PACKAGE__->set_primary_key("auto_pfamb_reg");
__PACKAGE__->has_many(
  "pdb_pfamb_regs",
  "PfamLive::PdbPfambReg",
  { "foreign.auto_pfamb_reg" => "self.auto_pfamb_reg" },
);
__PACKAGE__->belongs_to(
  "auto_pfamseq",
  "PfamLive::Pfamseq",
  { auto_pfamseq => "auto_pfamseq" },
);
__PACKAGE__->belongs_to(
  "auto_pfamb",
  "PfamLive::Pfamb",
  { auto_pfamb => "auto_pfamb" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3R9Gvi/U6k8GVNzQYk4grw

__PACKAGE__->has_one( pfamb =>  'PfamLive::Pfamb',
                      { 'foreign.auto_pfamb'  => 'self.auto_pfamb' },
                                            { proxy => [ qw( auto_pfamb pfamb_acc pfamb_id ) ] } );

# You can replace this text with custom content, and it will be preserved on regeneration
1;
