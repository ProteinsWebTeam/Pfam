package iPfamDB::Interface;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("interface");
__PACKAGE__->add_columns(
  "structure_accession",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "chain_a",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 1 },
  "chain_b",
  { data_type => "CHAR", default_value => "", is_nullable => 0, size => 1 },
  "residue_a",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "residue_b",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "bond",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 6 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-19 10:48:40
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ZLF0lyoi9ZgvK01LIBEEjA



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
