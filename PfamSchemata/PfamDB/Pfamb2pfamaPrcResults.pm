package PfamDB::Pfamb2pfamaPrcResults;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamB2pfamA_PRC_results");
__PACKAGE__->add_columns(
  "auto_pfamb",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 6 },
  "model_start1",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "model_end1",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "length1",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "align1",
  {
    data_type => "BLOB",
    default_value => undef,
    is_nullable => 0,
    size => 65535,
  },
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "model_start2",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "model_end2",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "length2",
  { data_type => "MEDIUMINT", default_value => 0, is_nullable => 0, size => 8 },
  "align2",
  {
    data_type => "BLOB",
    default_value => undef,
    is_nullable => 0,
    size => 65535,
  },
  "evalue",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 0,
    size => 25,
  },
);
__PACKAGE__->belongs_to(
  "auto_pfamb",
  "PfamDB::Pfamb",
  { auto_pfamb => "auto_pfamb" },
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamDB::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:pi1nvCKh2b4xKztMADVOIQ


__PACKAGE__->has_one(
  "pfamA",
  "PfamDB::Pfama",
  { "foreign.auto_pfama" => "self.auto_pfama" },
  { proxy => [ qw( pfama_id pfama_acc ) ] }
);

__PACKAGE__->has_one(
  "pfamB",
  "PfamDB::Pfamb",
  { "foreign.auto_pfamb" => "self.auto_pfamb" },
  { proxy => [ qw( pfamb_id pfamb_acc ) ] } );


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
