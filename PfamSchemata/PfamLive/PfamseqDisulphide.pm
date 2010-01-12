package PfamLive::PfamseqDisulphide;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamseq_disulphide");
__PACKAGE__->add_columns(
  "auto_pfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "bond_start",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "bond_end",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
);
__PACKAGE__->belongs_to(
  "auto_pfamseq",
  "PfamLive::Pfamseq",
  { auto_pfamseq => "auto_pfamseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:7GcwTXLAQQoguW5PwzUijg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
