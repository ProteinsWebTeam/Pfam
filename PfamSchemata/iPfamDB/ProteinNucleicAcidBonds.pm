package iPfamDB::ProteinNucleicAcidBonds;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("protein_nucleic_acid_bonds");
__PACKAGE__->add_columns(
  "nucleic_acid_atom",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "protein_atom",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "bond_type",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
  "distance",
  { data_type => "FLOAT", default_value => "", is_nullable => 0, size => 32 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2007-10-26 10:08:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:6UzqHGNE95UMbB+s2nkn2g


# You can replace this text with custom content, and it will be preserved on regeneration
1;
