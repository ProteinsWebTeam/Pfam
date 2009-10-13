package PfamDB::ClanAlignmentsAndRelationships;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("clan_alignments_and_relationships");
__PACKAGE__->add_columns(
  "auto_clan",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 4 },
  "alignment",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "relationship",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "image_map",
  {
    data_type => "BLOB",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "stockholm",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
);
__PACKAGE__->belongs_to("auto_clan", "PfamDB::Clans", { auto_clan => "auto_clan" });


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-08-04 15:06:30
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:rbOAcSl6VxrgDFbjSuSoSw

__PACKAGE__->set_primary_key("auto_clan");

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
