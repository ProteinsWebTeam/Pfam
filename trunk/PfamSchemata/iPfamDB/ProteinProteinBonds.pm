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
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 1 },
);
__PACKAGE__->add_unique_constraint(
  "protein_protein_bonds_unique",
  ["atom_a", "atom_b", "bond_type"],
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:1MRhTmRWNMlULuUNcfTKdg


# You can replace this text with custom content, and it will be preserved on regeneration

__PACKAGE__->add_unique_constraint(
     protein_protein_bonds_unique => [ qw(atom_a atom_b bond_type intrachain) ],
);

__PACKAGE__->has_one( "atomA" => "iPfamDB::ProteinIntAtoms",
                      {"foreign.atom_acc"  => "self.atom_a"});
                      
__PACKAGE__->has_one( "atomB" => "iPfamDB::ProteinIntAtoms",
                      {"foreign.atom_acc"  => "self.atom_b"});
                      
1;
