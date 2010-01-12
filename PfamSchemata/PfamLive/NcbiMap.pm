package PfamLive::NcbiMap;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ncbi_map");
__PACKAGE__->add_columns(
  "auto_pfamseq",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "gi",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
);
__PACKAGE__->belongs_to("gi", "PfamLive::NcbiSeq", { gi => "gi" });
__PACKAGE__->belongs_to(
  "auto_pfamseq",
  "PfamLive::Pfamseq",
  { auto_pfamseq => "auto_pfamseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:zhJf3so7+nfGFwD9i/bK0A


# You can replace this text with custom content, and it will be preserved on regeneration
1;
