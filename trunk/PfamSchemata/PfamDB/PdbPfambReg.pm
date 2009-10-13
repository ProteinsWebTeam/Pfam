package PfamDB::PdbPfambReg;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pdb_pfamB_reg");
__PACKAGE__->add_columns(
  "auto_pdb_reg",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 15 },
  "auto_pfamb_reg",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 15 },
  "pdb_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "auto_pfamb",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 6 },
  "auto_pfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
  "chain",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 4 },
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
);
__PACKAGE__->set_primary_key("auto_pdb_reg");
__PACKAGE__->belongs_to(
  "auto_pfamseq",
  "PfamDB::Pfamseq",
  { auto_pfamseq => "auto_pfamseq" },
);
__PACKAGE__->belongs_to(
  "auto_pfamb_reg",
  "PfamDB::PfambReg",
  { auto_pfamb_reg => "auto_pfamb_reg" },
);
__PACKAGE__->belongs_to(
  "auto_pfamb",
  "PfamDB::Pfamb",
  { auto_pfamb => "auto_pfamb" },
);
__PACKAGE__->belongs_to("pdb_id", "PfamDB::Pdb", { pdb_id => "pdb_id" });


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-08-18 18:25:15
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:tqVhmHTcMKSkHuXPCdPwrw


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
