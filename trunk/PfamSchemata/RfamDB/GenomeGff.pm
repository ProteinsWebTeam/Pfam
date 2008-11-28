package RfamDB::GenomeGff;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("genome_gff");
__PACKAGE__->add_columns(
  "auto_genome",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "gff3",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
);
__PACKAGE__->belongs_to(
  "auto_genome",
  "RfamDB::GenomeEntry",
  { auto_genome => "auto_genome" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-11-28 14:26:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:C77P5OwoXI/M6Z+WPS+Zmg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
