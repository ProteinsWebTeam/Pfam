package iPfamDB::Pli;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pli");
__PACKAGE__->add_columns(
  "protein_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
  "internal_ligand_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:1PxZ52wct+//Zie+ZsU1/g


# You can replace this text with custom content, and it will be preserved on regeneration
1;
