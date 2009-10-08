package PfamDB::PfamaLiteratureReferences;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA_literature_references");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "auto_lit",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "comment",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "order_added",
  { data_type => "TINYINT", default_value => undef, is_nullable => 1, size => 4 },
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamDB::Pfama",
  { auto_pfama => "auto_pfama" },
);
__PACKAGE__->belongs_to(
  "auto_lit",
  "PfamDB::LiteratureReferences",
  { auto_lit => "auto_lit" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:MlA04Na0y7RGKlO2+mXbVQ

__PACKAGE__->has_one( "literature" => "PfamDB::LiteratureReferences",
           {"foreign.auto_lit"  => "self.auto_lit"},
                     {proxy => [qw/pmid title author journal pmid/]});
# You can replace this text with custom content, and it will be preserved on regeneration
1;
