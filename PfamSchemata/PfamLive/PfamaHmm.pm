package PfamLive::PfamaHmm;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA_HMM");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "hmm",
  {
    data_type => "MEDIUMBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "logo",
  {
    data_type => "MEDIUMBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ZjiKVrRdqOcEOe1EVE9Krg
__PACKAGE__->set_primary_key("auto_pfama");

# You can replace this text with custom content, and it will be preserved on regeneration
1;
