package PfamDB::Version;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("VERSION");
__PACKAGE__->add_columns(
  "pfam_release",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "pfam_release_date",
  { data_type => "DATE", default_value => undef, is_nullable => 1, size => 10 },
  "swiss_prot_version",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "trembl_version",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "hmmer_version",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "pfama_coverage",
  { data_type => "FLOAT", default_value => undef, is_nullable => 1, size => 32 },
  "pfamb_additional_coverage",
  { data_type => "FLOAT", default_value => undef, is_nullable => 1, size => 32 },
  "pfama_residue_coverage",
  { data_type => "FLOAT", default_value => undef, is_nullable => 1, size => 32 },
  "pfamb_additional_residue_coverage",
  { data_type => "FLOAT", default_value => undef, is_nullable => 1, size => 32 },
  "number_families",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:9q+Z/Gv/sJhTPVKC1L56tg

__PACKAGE__->set_primary_key( qw/pfam_release swiss_prot_version trembl_version hmmer_version/);

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
