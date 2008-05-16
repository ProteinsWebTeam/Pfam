package iPfamDB::PdbConnectivity;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_connectivity");
__PACKAGE__->add_columns(
  "ligand_id",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "three_letter_code",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 3 },
  "atom1_name",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
  "atom2_name",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:h3ZRVxgPxvIFBrtWwq22uQ



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
