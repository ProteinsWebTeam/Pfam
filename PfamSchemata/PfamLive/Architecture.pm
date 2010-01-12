package PfamLive::Architecture;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("architecture");
__PACKAGE__->add_columns(
  "auto_architecture",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "architecture",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "type_example",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "no_seqs",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 8 },
  "architecture_acc",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
);
__PACKAGE__->set_primary_key("auto_architecture");
__PACKAGE__->has_many(
  "clan_architectures",
  "PfamLive::ClanArchitecture",
  { "foreign.auto_architecture" => "self.auto_architecture" },
);
__PACKAGE__->has_many(
  "pfama_architectures",
  "PfamLive::PfamaArchitecture",
  { "foreign.auto_architecture" => "self.auto_architecture" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:SJDplfNiDy7xgfvURK6oHA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
