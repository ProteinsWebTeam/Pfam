package iPfamDB::PdbConnectivity;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_connectivity");
__PACKAGE__->add_columns(
  "ligand_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "three_letter_code",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 3 },
  "atom1_name",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
  "atom2_name",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
);
__PACKAGE__->belongs_to(
  "ligand_id",
  "iPfamDB::LigandChemistry",
  { ligand_id => "ligand_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:otm2ENqNg9whSYqJN85ksA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
