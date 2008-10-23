package iPfamDB::LigandSummary;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ligand_summary");
__PACKAGE__->add_columns(
  "lig_code",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 10 },
  "ligand_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "three_letter_code",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 3 },
  "name",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "formula",
  { data_type => "TEXT", default_value => "", is_nullable => 0, size => 65535 },
  "domCount",
  { data_type => "INT", default_value => "", is_nullable => 1, size => 5 },
  "image",
  { data_type => "BLOB", default_value => "", is_nullable => 1, size => 65535 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:f+CT7ISNNAKTzhiWwlC4aQ


__PACKAGE__->belongs_to( 
  'chemistry' => 'iPfamDB::LigandChemistry',
  { 'foreign.lig_code' => 'self.lig_code' }
);


=head2 TABLE DEFINITION

+-------------------+-------------+------+-----+---------+-------+
| Field             | Type        | Null | Key | Default | Extra |
+-------------------+-------------+------+-----+---------+-------+
| lig_code          | varchar(10) | NO   |     |         |       |
| ligand_id         | int(10)     | NO   | MUL |         |       |
| three_letter_code | char(3)     | NO   | MUL |         |       |
| name              | text        | NO   |     |         |       |
| formula           | text        | NO   |     |         |       |
| domCount          | int(10)     | YES  |     | NULL    |       |
| image             | blob        | YES  |     | NULL    |       |
+-------------------+-------------+------+-----+---------+-------+

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
