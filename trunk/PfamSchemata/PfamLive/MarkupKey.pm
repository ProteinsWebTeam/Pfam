package PfamLive::MarkupKey;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("markup_key");
__PACKAGE__->add_columns(
  "auto_markup",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 3 },
  "label",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 35,
  },
);
__PACKAGE__->set_primary_key("auto_markup");
__PACKAGE__->has_many(
  "pfamseq_markups",
  "PfamLive::PfamseqMarkup",
  { "foreign.auto_markup" => "self.auto_markup" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:OFAinX8ugLyBWSmZKr0V0w


# You can replace this text with custom content, and it will be preserved on regeneration
1;
