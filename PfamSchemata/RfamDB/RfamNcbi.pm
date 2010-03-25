package RfamDB::RfamNcbi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("rfam_ncbi");
__PACKAGE__->add_columns(
  "ncbi_code",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "auto_rfam",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "rfam_acc",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 7 },
  "rfam_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 40,
  },
);
__PACKAGE__->set_primary_key("ncbi_code", "auto_rfam");
__PACKAGE__->belongs_to(
  "ncbi_code",
  "RfamDB::TaxonomyWebsearch",
  { ncbi_code => "ncbi_taxid" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2010-01-12 10:09:31
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:LsRi7VzSY2s/aOTfg/BYWQ

__PACKAGE__->many_to_many( ncbi_code => "RfamDB::TaxonomyWebsearch", 'ncbi_taxid' );

__PACKAGE__->has_one(
  "tax",
  "RfamDB::TaxonomyWebsearch",
  { "foreign.ncbi_taxid" => "self.ncbi_code" },
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
