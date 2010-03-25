package RfamDB::TaxonomyWebsearch;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("taxonomy_websearch");
__PACKAGE__->add_columns(
  "ncbi_taxid",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "species",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "taxonomy",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "lft",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "rgt",
  { data_type => "INT", default_value => undef, is_nullable => 1, size => 10 },
  "parent",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "level",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
);
__PACKAGE__->set_primary_key("ncbi_taxid");
__PACKAGE__->has_many(
  "rfam_ncbis",
  "RfamDB::RfamNcbi",
  { "foreign.ncbi_code" => "self.ncbi_taxid" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2010-01-12 10:09:31
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:VtUucSeM2A0lypWDY4CZtA


__PACKAGE__->many_to_many( ncbi_code => "RfamDB::RfamNcbi", 'ncbi_code' );

__PACKAGE__->set_primary_key( qw/ncbi_taxid/ );

__PACKAGE__->has_many(
  "rfam_ncbi",
  "RfamDB::RfamNcbi",
  { "foreign.ncbi_code" => "self.ncbi_taxid" },
);

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
