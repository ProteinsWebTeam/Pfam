package PfamDB::NcbiSeq;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("ncbi_seq");
__PACKAGE__->add_columns(
  "gi",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "secondary_acc",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 12,
  },
  "tertiary_acc",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 23,
  },
  "md5",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 32,
  },
  "description",
  {
    data_type => "TEXT",
    default_value => undef,
    is_nullable => 0,
    size => 65535,
  },
  "length",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "sequence",
  {
    data_type => "BLOB",
    default_value => undef,
    is_nullable => 0,
    size => 65535,
  },
);
__PACKAGE__->set_primary_key("gi");
__PACKAGE__->has_many(
  "metaseq_ncbi_maps",
  "PfamDB::MetaseqNcbiMap",
  { "foreign.gi" => "self.gi" },
);
__PACKAGE__->has_many(
  "ncbi_maps",
  "PfamDB::NcbiMap",
  { "foreign.gi" => "self.gi" },
);
__PACKAGE__->has_many(
  "ncbi_pfama_regs",
  "PfamDB::NcbiPfamaReg",
  { "foreign.gi" => "self.gi" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:eX5ZlLLYr7gG802E8bTZJg


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
