package PfamLive::MetaseqNcbiMap;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("metaseq_ncbi_map");
__PACKAGE__->add_columns(
  "gi",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_metaseq",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
);
__PACKAGE__->belongs_to("gi", "PfamLive::NcbiSeq", { gi => "gi" });
__PACKAGE__->belongs_to(
  "auto_metaseq",
  "PfamLive::Metaseq",
  { auto_metaseq => "auto_metaseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:y/NuFUezv0JeCCiKzZXoOA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
