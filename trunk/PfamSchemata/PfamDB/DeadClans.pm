package PfamDB::DeadClans;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("dead_clans");
__PACKAGE__->add_columns(
  "clan_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
  "clan_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 40 },
  "clan_description",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "clan_membership",
  {
    data_type => "LONGTEXT",
    default_value => "",
    is_nullable => 0,
    size => 4294967295,
  },
  "comment",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "forward_to",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 6 },
  "user",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "killed",
  {
    data_type => "TIMESTAMP",
    default_value => "CURRENT_TIMESTAMP",
    is_nullable => 0,
    size => 14,
  },
);
__PACKAGE__->add_unique_constraint("clan_acc", ["clan_acc"]);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-07-24 17:53:29
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ZSucuua1lEXHIR0Yy6TUiA

__PACKAGE__->set_primary_key(qw(clan_acc));

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
