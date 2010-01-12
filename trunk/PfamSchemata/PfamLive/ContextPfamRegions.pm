package PfamLive::ContextPfamRegions;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("context_pfam_regions");
__PACKAGE__->add_columns(
  "auto_pfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "seq_start",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "seq_end",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "domain_score",
  {
    data_type => "DOUBLE",
    default_value => "0.00",
    is_nullable => 0,
    size => 64,
  },
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);
__PACKAGE__->belongs_to(
  "auto_pfamseq",
  "PfamLive::Pfamseq",
  { auto_pfamseq => "auto_pfamseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ebhA+vVqlnIR5Scj4/3qqg

__PACKAGE__->has_one(
  pfama =>  'PfamLive::Pfama',
  { 'foreign.auto_pfama'  => 'self.auto_pfama' },
  { proxy      => [qw( pfama_id pfama_acc description)] }
);
# You can replace this text with custom content, and it will be preserved on regeneration
1;
