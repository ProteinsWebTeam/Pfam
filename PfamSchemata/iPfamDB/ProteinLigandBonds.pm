package iPfamDB::ProteinLigandBonds;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("protein_ligand_bonds");
__PACKAGE__->add_columns(
  "protein_atom",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "ligand_atom",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "bond_type",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
  "distance",
  { data_type => "FLOAT", default_value => "", is_nullable => 0, size => 32 },
);
__PACKAGE__->add_unique_constraint(
  "protein_ligand_bonds_unique",
  ["protein_atom", "ligand_atom", "bond_type"],
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:bcwo2jBv3LfwNq9PCmfsNQ


# You can replace this text with custom content, and it will be preserved on regeneration

__PACKAGE__->add_unique_constraint(
    protein_ligand_bonds_unique => [ qw(protein_atom ligand_atom bond_type) ],
);

__PACKAGE__->has_one( 'proteinAtom' => 'iPfamDB::ProteinIntAtoms',
                      {"foreign.atom_acc"  => 'self.protein_atom'},
                      { 'proxy' => [ qw(protien_acc atom_number) ] });
                      
__PACKAGE__->has_one( 'ligandAtom' => 'iPfamDB::LigandIntAtoms',
                      { 'foreign.atom_acc'  => 'self.ligand_atom'},
                      { 'proxy' => [ qw(internal_ligand_id atom_number) ] } );
1;
