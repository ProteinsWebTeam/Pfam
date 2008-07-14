package RfamDB::ChromosomeBuild;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("chromosome_build");
__PACKAGE__->add_columns(
  "auto_genome",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_rfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "xsome_start",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
  "xsome_end",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
  "clone_start",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
  "clone_end",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
  "strand",
  { data_type => "CHAR", default_value => undef, is_nullable => 1, size => 2 },
);
__PACKAGE__->set_primary_key("auto_genome", "auto_rfamseq");
__PACKAGE__->belongs_to(
  "auto_rfamseq",
  "RfamDB::Rfamseq",
  { auto_rfamseq => "auto_rfamseq" },
);
__PACKAGE__->belongs_to(
  "auto_genome",
  "RfamDB::GenomeEntry",
  { auto_genome => "auto_genome" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-14 20:19:36
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ac88Sd7GvRvcULkNmspu9Q


# You can replace this text with custom content, and it will be preserved on regeneration
1;
