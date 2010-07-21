package iPfamDB::PdbAuthor;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_author");
__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "author_order",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 3 },
  "name",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:OhAU7GUkqcrDYysCEZuU9Q


# You can replace this text with custom content, and it will be preserved on regeneration
1;
