package PfamDB::NcbiPfamaReg;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ncbi_pfamA_reg");
__PACKAGE__->add_columns(
  "auto_ncbi_pfama_reg",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 15 },
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "gi",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "seq_start",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "seq_end",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "ali_start",
  { data_type => "MEDIUMINT", default_value => "", is_nullable => 0, size => 8 },
  "ali_end",
  { data_type => "MEDIUMINT", default_value => "", is_nullable => 0, size => 8 },
  "model_start",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "model_end",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "domain_bits_score",
  {
    data_type => "DOUBLE",
    default_value => "0.0000",
    is_nullable => 0,
    size => 64,
  },
  "domain_evalue_score",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 15 },
  "sequence_bits_score",
  {
    data_type => "DOUBLE",
    default_value => "0.0000",
    is_nullable => 0,
    size => 64,
  },
  "sequence_evalue_score",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 15 },
  "cigar",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 1,
    size => 65535,
  },
  "in_full",
  { data_type => "TINYINT", default_value => 0, is_nullable => 0, size => 4 },
  "tree_order",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 9,
  },
);
__PACKAGE__->set_primary_key("auto_ncbi_pfama_reg");
__PACKAGE__->belongs_to("gi", "PfamDB::NcbiSeq", { gi => "gi" });
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamDB::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-07-23 10:58:05
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:05W2TsuGm1c3YNckCP0F8g


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
