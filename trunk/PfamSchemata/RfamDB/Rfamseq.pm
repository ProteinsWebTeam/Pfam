package RfamDB::Rfamseq;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("rfamseq");
__PACKAGE__->add_columns(
  "auto_rfamseq",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "rfamseq_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 12 },
  "rfamseq_acc",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 12,
  },
  "description",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 255 },
  "species",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 100 },
  "taxonomy",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "version",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 12,
  },
  "previous_acc",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "taxon",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
);
__PACKAGE__->set_primary_key("auto_rfamseq");
__PACKAGE__->add_unique_constraint("rfamseq_id", ["rfamseq_id"]);
__PACKAGE__->has_many(
  "chromosome_builds",
  "RfamDB::ChromosomeBuild",
  { "foreign.auto_rfamseq" => "self.auto_rfamseq" },
);
__PACKAGE__->has_many(
  "rfam_reg_seeds",
  "RfamDB::RfamRegSeed",
  { "foreign.auto_rfamseq" => "self.auto_rfamseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-09 20:46:10
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:BDAYT5QVlPQ0bmNjWSE/hw

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk), 
         Jennifer Daub (jd7@sanger.ac.uk)

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
