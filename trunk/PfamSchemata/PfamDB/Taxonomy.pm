use utf8;
package PfamDB::Taxonomy;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::Taxonomy

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<taxonomy>

=cut

__PACKAGE__->table("taxonomy");

=head1 ACCESSORS

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 1

=head2 species

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 1

=head2 lft

  data_type: 'integer'
  is_nullable: 1

=head2 rgt

  data_type: 'integer'
  is_nullable: 1

=head2 parent

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 level

  data_type: 'varchar'
  is_nullable: 1
  size: 200

=head2 minimal

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 0

=head2 rank

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=cut

__PACKAGE__->add_columns(
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 1,
  },
  "species",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 1 },
  "lft",
  { data_type => "integer", is_nullable => 1 },
  "rgt",
  { data_type => "integer", is_nullable => 1 },
  "parent",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "level",
  { data_type => "varchar", is_nullable => 1, size => 200 },
  "minimal",
  { data_type => "tinyint", default_value => 0, is_nullable => 0 },
  "rank",
  { data_type => "varchar", is_nullable => 1, size => 100 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:qaELWqynSSsQaMkxSJrc0g

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
