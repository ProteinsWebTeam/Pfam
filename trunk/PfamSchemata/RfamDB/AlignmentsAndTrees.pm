package RfamDB::AlignmentsAndTrees;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("alignments_and_trees");
__PACKAGE__->add_columns(
  "auto_rfam",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "type",
  { data_type => "ENUM", default_value => "", is_nullable => 0, size => 4 },
  "alignment",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "tree",
  {
    data_type => "LONGBLOB",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "treemethod",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "average_length",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "percent_id",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "number_of_sequences",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
  "most_unrelated_pair",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
);
__PACKAGE__->set_primary_key("auto_rfam", "type");
__PACKAGE__->belongs_to("auto_rfam", "RfamDB::Rfam", { auto_rfam => "auto_rfam" });


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2009-01-14 13:54:47
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:OmHaXgVYKVcKrtb05JfK4Q

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
