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
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "distance",
  { data_type => "FLOAT", default_value => "", is_nullable => 0, size => 32 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:rbZ4qEGejUGhzq587pGApg


# You can replace this text with custom content, and it will be preserved on regeneration
__PACKAGE__->add_unique_constraint(
    protein_na_bonds_unique => [ qw(protein_atom nucleic_acid_atom bond_type) ],
);

__PACKAGE__->has_one( 'proteinAtom' => 'iPfamDB::ProteinIntAtoms',
                      {"foreign.atom_acc"  => 'self.protein_atom'},
                      { 'proxy' => [ qw(protien_acc atom_number) ] });
                      
__PACKAGE__->has_one( 'nucleicAcidAtom' => 'iPfamDB::NucleicAcidIntAtoms',
                      { 'foreign.atom_acc'  => 'self.nucleic_acid_atom'},
                      { 'proxy' => [ qw(nucleic_acid_acc atom_number) ] } );
1;
