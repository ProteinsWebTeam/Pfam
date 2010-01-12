package PfamLive::GenomeSeqs;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("genome_seqs");
__PACKAGE__->add_columns(
  "ncbi_code",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_pfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "count",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:rmwLmNPw2My5fg6d9r4HVw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
