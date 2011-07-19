package PfamDB::Taxonomy;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("taxonomy");
__PACKAGE__->add_columns(
  "ncbi_taxid",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
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
__PACKAGE__->belongs_to(
  "ncbi_taxid",
  "PfamDB::NcbiTaxonomy",
  { ncbi_taxid => "ncbi_taxid" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:qsqa114ln2YRhFU0SMRhMQ

__PACKAGE__->has_one(
  "pfama_ncbi",
  "PfamDB::PfamaNcbi",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
);

__PACKAGE__->set_primary_key("ncbi_taxid");



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
