package PfamDB::DeadFamilies;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("dead_families");
__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
  "pfama_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 40 },
  "comment",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "forward_to",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 7 },
  "user",
  {
    data_type => "VARCHAR",
    default_value => "anon",
    is_nullable => 0,
    size => 10,
  },
  "killed",
  {
    data_type => "TIMESTAMP",
    default_value => "CURRENT_TIMESTAMP",
    is_nullable => 0,
    size => 14,
  },
);
__PACKAGE__->add_unique_constraint("pfamA_acc", ["pfama_acc"]);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-07-24 17:53:29
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3N/eAyuLsymFIg7SmhN79g


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
