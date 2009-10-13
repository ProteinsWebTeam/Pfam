package PfamDB::NestedLocations;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("nested_locations");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 5 },
  "nested_auto_pfama",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 5 },
  "nested_pfama_acc",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 0, size => 7 },
  "pfamseq_acc",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 0, size => 6 },
  "seq_version",
  { data_type => "TINYINT", default_value => undef, is_nullable => 1, size => 4 },
  "seq_start",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "seq_end",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "auto_pfamseq",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamDB::Pfama",
  { auto_pfama => "auto_pfama" },
);
__PACKAGE__->belongs_to(
  "auto_pfamseq",
  "PfamDB::Pfamseq",
  { auto_pfamseq => "auto_pfamseq" },
);

__PACKAGE__->might_have(
  "clan_membership" => 'PfamDB::ClanMembership',
			{ 'foreign.auto_pfama' => 'self.auto_pfama' } );




# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Ba+nSWA8RN4XuJHsxb1tag


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
