package RfamDB::PdbRfamReg;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_rfam_reg");
__PACKAGE__->add_columns(
  "auto_pdb_reg",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 15 },
  "auto_rfam",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "pdb_id",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 6 },
  "chain",
  {
    data_type => "VARCHAR",
    default_value => "NULL",
    is_nullable => 1,
    size => 4,
  },
  "pdb_res_start",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "pdb_res_end",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "rfamseq_acc",
  { data_type => "CHAR", default_value => undef, is_nullable => 1, size => 10 },
  "seq_start",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 19 },
  "seq_end",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 19 },
  "hex_colour",
  {
    data_type => "VARCHAR",
    default_value => "NULL",
    is_nullable => 1,
    size => 6,
  },
);
__PACKAGE__->set_primary_key("auto_pdb_reg");
__PACKAGE__->belongs_to("auto_rfam", "RfamDB::Rfam", { auto_rfam => "auto_rfam" });


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-09-25 21:50:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:6iZQ9x3Asiwpg1u+TlgD8w

# in order to be able to use this table in a keyword search plugin, we need to
# have access to three columns from the rfam table, so we add this extra
# relationship, with those columns proxied through
 
__PACKAGE__->belongs_to("auto_rfam", 
                        "RfamDB::Rfam",
                        { auto_rfam => "auto_rfam"},
                        { proxy => [ qw( rfam_acc 
                                         rfam_id
                                         description
                                       ) ] } );


# You can replace this text with custom content, and it will be preserved on regeneration
1;
