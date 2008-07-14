package RfamDB::RfamRegSeed;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("rfam_reg_seed");
__PACKAGE__->add_columns(
  "auto_rfam",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_rfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "seq_start",
  { data_type => "BIGINT", default_value => 0, is_nullable => 0, size => 19 },
  "seq_end",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 19 },
);
__PACKAGE__->belongs_to(
  "auto_rfamseq",
  "RfamDB::Rfamseq",
  { auto_rfamseq => "auto_rfamseq" },
);
__PACKAGE__->belongs_to("auto_rfam", "RfamDB::Rfam", { auto_rfam => "auto_rfam" });


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-14 20:19:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:gHsGVzgcpo6jqYQCj1WsOQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
