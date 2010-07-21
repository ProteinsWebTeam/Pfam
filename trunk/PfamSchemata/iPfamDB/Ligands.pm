package iPfamDB::Ligands;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ligands");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "ligand_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 10,
  },
  "ligand_number",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "chain",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
  "atom_start",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "atom_end",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "internal_ligand_id",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("internal_ligand_id");
__PACKAGE__->add_unique_constraint(
  "ligands_uniq_idx",
  ["accession", "ligand_id", "ligand_number", "chain"],
);
__PACKAGE__->has_many(
  "dlis",
  "iPfamDB::Dli",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);
__PACKAGE__->has_many(
  "dli_res",
  "iPfamDB::DliRes",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);
__PACKAGE__->has_many(
  "ligand_int_atoms",
  "iPfamDB::LigandIntAtoms",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);
__PACKAGE__->belongs_to(
  "accession",
  "iPfamDB::PdbChainData",
  { "internal_chain_accession" => "accession" },
);
__PACKAGE__->belongs_to(
  "ligand_id",
  "iPfamDB::LigandChemistry",
  { ligand_id => "ligand_id" },
);
__PACKAGE__->has_many(
  "plis",
  "iPfamDB::Pli",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);
__PACKAGE__->has_many(
  "pli_res",
  "iPfamDB::PliRes",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 15:16:33
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:XHIjI7D58Q3kZUHq8yUD7g
# These lines were loaded from '/software/pfam/Modules/PfamSchemata/iPfamDB/Ligands.pm' found in @INC.# They are now part of the custom portion of this file# for you to hand-edit.  If you do not either delete# this section or remove that file from @INC, this section# will be repeated redundantly when you re-create this# file again via Loader!
package iPfamDB::Ligands;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ligands");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "ligand_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "ligand_number",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "chain",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
  "atom_start",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "atom_end",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "internal_ligand_id",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
);
__PACKAGE__->set_primary_key("internal_ligand_id");
__PACKAGE__->has_many(
  "dlis",
  "iPfamDB::Dli",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);
__PACKAGE__->has_many(
  "dli_res",
  "iPfamDB::DliRes",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);
__PACKAGE__->has_many(
  "ligand_int_atoms",
  "iPfamDB::LigandIntAtoms",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);
__PACKAGE__->belongs_to(
  "ligand_id",
  "iPfamDB::LigandChemistry",
  { ligand_id => "ligand_id" },
);
__PACKAGE__->belongs_to(
  "accession",
  "iPfamDB::PdbChainData",
  { "internal_chain_accession" => "accession" },
);
__PACKAGE__->has_many(
  "plis",
  "iPfamDB::Pli",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);
__PACKAGE__->has_many(
  "pli_res",
  "iPfamDB::PliRes",
  { "foreign.internal_ligand_id" => "self.internal_ligand_id" },
);


# Created by DBIx::Class::Schema::Loader v0.04006 @ 2009-11-16 12:00:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:8M3jQJqRnUNjztYJXdmubw


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
# End of lines loaded from '/software/pfam/Modules/PfamSchemata/iPfamDB/Ligands.pm' 


# You can replace this text with custom content, and it will be preserved on regeneration
1;
