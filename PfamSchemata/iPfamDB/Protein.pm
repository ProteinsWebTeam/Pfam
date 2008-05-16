package iPfamDB::Protein;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("protein");
__PACKAGE__->add_columns(
  "accession",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "seq_version",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "md5",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 32 },
  "source_db",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "length",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "ncbi_code",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 11 },
  "sequence",
  { data_type => "BLOB", default_value => "", is_nullable => 0, size => 65535 },
);
__PACKAGE__->set_primary_key("accession");
__PACKAGE__->add_unique_constraint("proteins_accession_id_Idx", ["accession", "id"]);
__PACKAGE__->add_unique_constraint("proteins_accession_Idx", ["accession"]);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:EkB15hNDw3aoIz43wP43KQ



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
