package iPfamDB::ProteinIntAtoms;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("protein_int_atoms");
__PACKAGE__->add_columns(
  "protein_acc",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "atom_number",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "protein_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 20,
  },
  "residue",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "atom_acc",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "atom",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 3 },
);
__PACKAGE__->set_primary_key("atom_acc");
__PACKAGE__->belongs_to(
  "protein_acc",
  "iPfamDB::Protein",
  { accession => "protein_acc" },
);
__PACKAGE__->has_many(
  "protein_ligand_bonds",
  "iPfamDB::ProteinLigandBonds",
  { "foreign.protein_atom" => "self.atom_acc" },
);
__PACKAGE__->has_many(
  "protein_nucleic_acid_bonds",
  "iPfamDB::ProteinNucleicAcidBonds",
  { "foreign.protein_atom" => "self.atom_acc" },
);
__PACKAGE__->has_many(
  "protein_protein_bonds",
  "iPfamDB::ProteinProteinBonds",
  { "foreign.atom_a" => "self.atom_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:jqK/YZ+K0MbOqDAGpPJrYQ

__PACKAGE__->has_many(
  "protein_protein_bonds",
  "iPfamDB::ProteinProteinBonds",
  { "foreign.atom_a" => "self.atom_acc" },
);

# You can replace this text with custom content, and it will be preserved on regeneration
1;
