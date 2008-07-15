package RfamDB::RfamRegFull;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("rfam_reg_full");
__PACKAGE__->add_columns(
  "auto_rfam",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_rfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "auto_genome",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "seq_start",
  { data_type => "BIGINT", default_value => 0, is_nullable => 0, size => 19 },
  "seq_end",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 19 },
  "bits_score",
  {
    data_type => "DOUBLE",
    default_value => "0.00",
    is_nullable => 0,
    size => 64,
  },
);
__PACKAGE__->belongs_to("auto_rfam", "RfamDB::Rfam", { auto_rfam => "auto_rfam" });
__PACKAGE__->belongs_to(
  "auto_rfamseq",
  "RfamDB::Rfamseq",
  { auto_rfamseq => "auto_rfamseq" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-07-14 20:19:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:H3paAvZvLAVsRKUl/fkJ6Q

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
