package iPfamDB::PdbConnectivity;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_connectivity");
__PACKAGE__->add_columns(
  "ligand_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "three_letter_code",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 3 },
  "atom1_name",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
  "atom2_name",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:h3ZRVxgPxvIFBrtWwq22uQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
