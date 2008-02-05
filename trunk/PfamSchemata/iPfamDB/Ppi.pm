package iPfamDB::Ppi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ppi");
__PACKAGE__->add_columns(
  "accession_a",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 6 },
  "accession_b",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 6 },
  "quality_control",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Sdc5UphUoLtMTSHcKe/dsQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
