package iPfamDB::ProteinProteinBonds;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("protein_protein_bonds");
__PACKAGE__->add_columns(
  "atom_a",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "atom_b",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "bond_type",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
  "distance",
  { data_type => "FLOAT", default_value => "", is_nullable => 0, size => 32 },
  "intrachain",
  { data_type => "TINYINT", default_value => "", is_nullable => 0, size => 1 },
);
__PACKAGE__->add_unique_constraint(
  "protein_protein_bonds_unique",
  ["atom_a", "atom_b", "bond_type"],
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:1MRhTmRWNMlULuUNcfTKdg


__PACKAGE__->add_unique_constraint(
     protein_protein_bonds_unique => [ qw(atom_a atom_b bond_type intrachain) ],
);

__PACKAGE__->has_one( "atomA" => "iPfamDB::ProteinIntAtoms",
                      {"foreign.atom_acc"  => "self.atom_a"});
                      
__PACKAGE__->has_one( "atomB" => "iPfamDB::ProteinIntAtoms",
                      {"foreign.atom_acc"  => "self.atom_b"});

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
