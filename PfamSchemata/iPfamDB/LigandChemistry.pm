package iPfamDB::LigandChemistry;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ligand_chemistry");
__PACKAGE__->add_columns(
  "ligand_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "lig_code",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "three_letter_code",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "one_letter_code",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 5 },
  "name",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "systematic_name",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "num_all_atoms",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "num_atoms_no_h",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "stereo_smiles",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "non_stereo_smiles",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "charge",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "category",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "formula",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "molecular_weight",
  { data_type => "FLOAT", default_value => undef, is_nullable => 1, size => 32 },
);
__PACKAGE__->set_primary_key("ligand_id");
__PACKAGE__->has_many(
  "ligand_summaries",
  "iPfamDB::LigandSummary",
  { "foreign.ligand_id" => "self.ligand_id" },
);
__PACKAGE__->has_many(
  "ligand_synonyms",
  "iPfamDB::LigandSynonyms",
  { "foreign.ligand_id" => "self.ligand_id" },
);
__PACKAGE__->has_many(
  "ligands",
  "iPfamDB::Ligands",
  { "foreign.ligand_id" => "self.ligand_id" },
);
__PACKAGE__->has_many(
  "pdb_connectivities",
  "iPfamDB::PdbConnectivity",
  { "foreign.ligand_id" => "self.ligand_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04006 @ 2009-11-16 12:00:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:/ued/8l8ydQ/FcMQonnw8Q


# You can replace this text with custom content, and it will be preserved on regeneration

=head1 AUTHOR

Prasad Gunasekaran, C<pg6@sanger.ac.uk>

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
