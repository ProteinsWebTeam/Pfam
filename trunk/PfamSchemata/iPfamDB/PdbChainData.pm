package iPfamDB::PdbChainData;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_chain_data");
__PACKAGE__->add_columns(
  "internal_chain_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "internal_chain_accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "original_chain",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 1 },
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
#  "accession",
#  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->add_unique_constraint("UQ_pdb_chain_data_1", ["internal_chain_accession"]);
__PACKAGE__->has_many(
  "ligands",
  "iPfamDB::Ligands",
  { "foreign.accession" => "self.internal_chain_accession" },
);
__PACKAGE__->has_many(
  "nucleic_acids",
  "iPfamDB::NucleicAcid",
  { "foreign.accession" => "self.internal_chain_accession" },
);
__PACKAGE__->belongs_to("accession", "iPfamDB::Pdb", { accession => "accession" });
__PACKAGE__->has_many(
  "proteins",
  "iPfamDB::Protein",
  { "foreign.accession" => "self.internal_chain_accession" },
);


# Created by DBIx::Class::Schema::Loader v0.04006 @ 2009-11-16 12:00:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:iVyx6tF8fOUpV0YDmm/PEA

__PACKAGE__->belongs_to("pdbtable", "iPfamDB::Pdb", { pdb_id => "pdb_id" });
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
