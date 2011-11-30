package PfamDB::ProteomeArchitecture;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("proteome_architecture");
__PACKAGE__->add_columns(
  "auto_proteome",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "auto_architecture",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
 "type_example",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "no_seqs",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 8 },
);
__PACKAGE__->belongs_to(
  "auto_proteome",
  "PfamDB::CompleteProteomes",
  { auto_proteome => "auto_proteome" },
);
__PACKAGE__->belongs_to(
  "auto_architecture",
  "PfamDB::Architecture",
  { auto_architecture => "auto_architecture" },
);
__PACKAGE__->has_one( 
  "storable",
  "PfamDB::PfamAnnseq",
  { "foreign.auto_pfamseq" => "self.type_example" },
);

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
