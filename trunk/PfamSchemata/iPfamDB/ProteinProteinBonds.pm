package iPfamDB::ProteinProteinBonds;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("protein_protein_bonds");
__PACKAGE__->add_columns(
  "atom_a",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "atom_b",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "bond_type",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
  "distance",
  { data_type => "FLOAT", default_value => "", is_nullable => 0, size => 32 },
  "intrachain",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 3 },
);
__PACKAGE__->add_unique_constraint(
  "protein_protein_bonds_unique",
  ["atom_a", "atom_b", "bond_type"],
);
__PACKAGE__->belongs_to("atom_a", "iPfamDB::ProteinIntAtoms", { atom_acc => "atom_a" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:YuVbzRwYncv39rRJ2OdlUQ

__PACKAGE__->belongs_to("atom_b", "iPfamDB::ProteinIntAtoms", { atom_acc => "atom_b" });

# You can replace this text with custom content, and it will be preserved on regeneration
1;
