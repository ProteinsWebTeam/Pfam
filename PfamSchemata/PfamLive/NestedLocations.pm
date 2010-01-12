package PfamLive::NestedLocations;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nested_locations");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 5 },
  "nested_auto_pfama",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 5 },
  "nested_pfama_acc",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 0, size => 7 },
  "pfamseq_acc",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 0, size => 6 },
  "seq_version",
  { data_type => "TINYINT", default_value => undef, is_nullable => 1, size => 4 },
  "seq_start",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "seq_end",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "auto_pfamseq",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
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

__PACKAGE__->might_have(
  "clan_membership" => 'PfamLive::ClanMembership',
			{ 'foreign.auto_pfama' => 'self.auto_pfama' } );




# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Ba+nSWA8RN4XuJHsxb1tag


# You can replace this text with custom content, and it will be preserved on regeneration
1;
