package PfamLive::NestedDomains;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nested_domains");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "nests_auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:DBuHa9rAEzhtkbyvV3sRIg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
