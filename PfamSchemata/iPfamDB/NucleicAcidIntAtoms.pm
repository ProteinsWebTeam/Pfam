package iPfamDB::NucleicAcidIntAtoms;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nucleic_acid_int_atoms");
__PACKAGE__->add_columns(
  "nucleic_acid_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 6 },
  "atom_number",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "nucleic_acid_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
  "base",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "base_name",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "atom",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 3 },
  "atom_acc",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->belongs_to(
  "nucleic_acid_acc",
  "iPfamDB::NucleicAcid",
  { accession => "nucleic_acid_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 16:29:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:d2UklZKBAkBOO4ts+LrlDg

__PACKAGE__->set_primary_key("atom_acc");

__PACKAGE__->has_many(
  "nucleic_acid__bonds",
  "iPfamDB::ProteinNucleicAcidBonds",
  { "foreign.nucleic_acid_atom" => "self.atom_acc" },
);

# You can replace this text with custom content, and it will be preserved on regeneration
1;
