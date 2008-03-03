package iPfamDB::PdbAuthor;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_author");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "author_order",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "last_name",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "name_initials",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:bVVBZm1/LV0V+Sul7RhgFA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
