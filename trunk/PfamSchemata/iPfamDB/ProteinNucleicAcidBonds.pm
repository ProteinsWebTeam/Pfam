package iPfamDB::ProteinNucleicAcidBonds;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("protein_nucleic_acid_bonds");
__PACKAGE__->add_columns(
  "nucleic_acid_atom",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "protein_atom",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "bond_type",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "distance",
  { data_type => "FLOAT", default_value => "", is_nullable => 0, size => 32 },
);
__PACKAGE__->belongs_to(
  "protein_atom",
  "iPfamDB::ProteinIntAtoms",
  { atom_acc => "protein_atom" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:jeSZW4bmI2tUHhcuW4uUbA

__PACKAGE__->belongs_to(
  "nucleic_acid_atom",
  "iPfamDB::NucleicAcidIntAtoms",
  { atom_acc => "nucleic_acid_atom" },
);

# You can replace this text with custom content, and it will be preserved on regeneration
1;
