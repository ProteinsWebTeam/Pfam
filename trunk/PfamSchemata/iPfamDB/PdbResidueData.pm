package iPfamDB::PdbResidueData;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_residue_data");
__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "msd_chain",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 4 },
  "chain",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
  "serial",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "pdb_residue",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "pdb_coordinate",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "pdb_insert_code",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 1 },
  "uniprot_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "seq_version",
  { data_type => "TINYINT", default_value => undef, is_nullable => 1, size => 4 },
  "uniprot_residue",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "uniprot_coordinate",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "dssp_code",
  { data_type => "VARCHAR", default_value => 0, is_nullable => 0, size => 4 },
);
__PACKAGE__->belongs_to("pdb_id", "iPfamDB::Pdb", { pdb_id => "pdb_id" });


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2010-07-21 15:16:33
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:4psOoFh324Kah0/+wiiLWw
# These lines were loaded from '/software/pfam/Modules/PfamSchemata/iPfamDB/PdbResidueData.pm' found in @INC.# They are now part of the custom portion of this file# for you to hand-edit.  If you do not either delete# this section or remove that file from @INC, this section# will be repeated redundantly when you re-create this# file again via Loader!
package iPfamDB::PdbResidueData;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_residue_data");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "chain",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 1 },
  "uniprot_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "pdb_coordinate",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "pdb_residue",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "dssp_code",
  { data_type => "VARCHAR", default_value => 0, is_nullable => 0, size => 4 },
  "uniprot_coordinate",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "uniprot_residue",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
);
__PACKAGE__->belongs_to("accession", "iPfamDB::Pdb", { accession => "accession" });


# Created by DBIx::Class::Schema::Loader v0.04006 @ 2009-11-16 12:00:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:cv2H05IQjiKrZH1afIaSrQ


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
# End of lines loaded from '/software/pfam/Modules/PfamSchemata/iPfamDB/PdbResidueData.pm' 


# You can replace this text with custom content, and it will be preserved on regeneration
1;
