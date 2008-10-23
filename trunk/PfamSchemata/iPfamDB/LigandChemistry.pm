package iPfamDB::LigandChemistry;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ligand_chemistry");
__PACKAGE__->add_columns(
  "ligand_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
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
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 11 },
  "category",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "formula",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "molecular_weight",
  { data_type => "FLOAT", default_value => undef, is_nullable => 1, size => 32 },
);
 __PACKAGE__->add_unique_constraint("ligand_chemistry_ligand_id_idx", ["ligand_id"]);
__PACKAGE__->add_unique_constraint("ligand_chemistry_code_idx", ["lig_code"]);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:30W8zCkNvzz3IgHdy4L3AA


__PACKAGE__->set_primary_key("lig_code");

__PACKAGE__->might_have( 
  'summary' => 'iPfamDB::LigandSummary',
  { 'foreign.lig_code' => 'self.lig_code' }
);

__PACKAGE__->add_unique_constraint( "ligand_chemistry_three_letter_code", ["three_letter_code"]);

=head2 TABLE DEFINITION

+-------------------+------------------+------+-----+---------+-------+
| Field             | Type             | Null | Key | Default | Extra |
+-------------------+------------------+------+-----+---------+-------+
| ligand_id         | int(11)          | NO   | UNI |         |       |
| lig_code          | varchar(10)      | NO   | PRI |         |       |
| three_letter_code | char(3)          | NO   | MUL |         |       |
| one_letter_code   | varchar(5)       | YES  |     | NULL    |       |
| name              | text             | NO   |     |         |       |
| systematic_name   | text             | NO   |     |         |       |
| num_all_atoms     | int(10) unsigned | YES  |     | NULL    |       |
| num_atoms_no_H    | int(10) unsigned | YES  |     | NULL    |       |
| stereo_smiles     | text             | NO   |     |         |       |
| non_stereo_smiles | text             | YES  |     | NULL    |       |
| charge            | int(11)          | YES  |     | NULL    |       |
| category          | text             | NO   |     |         |       |
| formula           | text             | NO   |     |         |       |
| molecular_weight  | float            | YES  |     | NULL    |       |
+-------------------+------------------+------+-----+---------+-------+

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

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
