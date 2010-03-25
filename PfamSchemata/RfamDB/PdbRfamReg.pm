package RfamDB::PdbRfamReg;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_rfam_reg");
__PACKAGE__->add_columns(
  "auto_pdb_reg",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 15 },
  "auto_rfam",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "pdb_id",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 6 },
  "chain",
  {
    data_type => "VARCHAR",
    default_value => "NULL",
    is_nullable => 1,
    size => 4,
  },
  "pdb_res_start",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "pdb_res_end",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "rfamseq_acc",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 50,
  },
  "seq_start",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 19 },
  "seq_end",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 19 },
  "hex_colour",
  {
    data_type => "VARCHAR",
    default_value => "NULL",
    is_nullable => 1,
    size => 6,
  },
);
__PACKAGE__->set_primary_key("auto_pdb_reg");
__PACKAGE__->belongs_to("auto_rfam", "RfamDB::Rfam", { auto_rfam => "auto_rfam" });


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2010-01-12 10:09:30
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:8ZXdFps78DpxX4TZNs8gOg

# in order to be able to use this table in a keyword search plugin, we need to
# have access to three columns from the rfam table, so we add this extra
# relationship, with those columns proxied through
 
__PACKAGE__->belongs_to("auto_rfam", 
                        "RfamDB::Rfam",
                        { auto_rfam => "auto_rfam"},
                        { proxy => [ qw( rfam_acc 
                                         rfam_id
                                         description
                                       ) ] } );

__PACKAGE__->belongs_to(
  "rfamseq",
  "RfamDB::Rfamseq",
  { rfamseq_acc => "rfamseq_acc" },
);

#__PACKAGE__->has_many(
#  "alignments_and_trees",
#  "RfamDB::AlignmentsAndTrees",
#  { "foreign.auto_rfam" => "self.auto_rfam" }
#);

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
