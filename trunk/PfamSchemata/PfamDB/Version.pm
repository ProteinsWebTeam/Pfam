use utf8;
package PfamDB::Version;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::Version

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<version>

=cut

__PACKAGE__->table("version");

=head1 ACCESSORS

=head2 pfam_release

  data_type: 'tinytext'
  is_nullable: 1

=head2 pfam_release_date

  data_type: 'date'
  datetime_undef_if_invalid: 1
  is_nullable: 1

=head2 swiss_prot_version

  data_type: 'tinytext'
  is_nullable: 1

=head2 trembl_version

  data_type: 'tinytext'
  is_nullable: 1

=head2 reference_proteome_version

  data_type: 'tinytext'
  is_nullable: 1

=head2 hmmer_version

  data_type: 'tinytext'
  is_nullable: 1

=head2 pfama_coverage

  data_type: 'float'
  is_nullable: 1
  size: [4,1]

=head2 pfama_residue_coverage

  data_type: 'float'
  is_nullable: 1
  size: [4,1]

=head2 number_families

  data_type: 'integer'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfam_release",
  { data_type => "tinytext", is_nullable => 1 },
  "pfam_release_date",
  { data_type => "date", datetime_undef_if_invalid => 1, is_nullable => 1 },
  "swiss_prot_version",
  { data_type => "tinytext", is_nullable => 1 },
  "trembl_version",
  { data_type => "tinytext", is_nullable => 1 },
  "reference_proteome_version",
  { data_type => "tinytext", is_nullable => 1 },
  "hmmer_version",
  { data_type => "tinytext", is_nullable => 1 },
  "pfama_coverage",
  { data_type => "float", is_nullable => 1, size => [4, 1] },
  "pfama_residue_coverage",
  { data_type => "float", is_nullable => 1, size => [4, 1] },
  "number_families",
  { data_type => "integer", is_nullable => 1 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-06-25 11:22:06
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:6H5/6fN2z1eMgqoyOLa30g

#__PACKAGE__->set_primary_key( qw/pfam_release swiss_prot_version trembl_version hmmer_version/);

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
