package PfamLive::PfamaTaxDepth;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA_tax_depth");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 5 },
  "root",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 24 },
  "count",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 6 },
  "common",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2010-04-12 15:54:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Dj6q96/m9JSeSsqR0jfPRg

# You can replace this text with custom content, and it will be preserved on regeneration
1;
