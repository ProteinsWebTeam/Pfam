use utf8;
package PfamDB::NcbiTaxonomy;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::NcbiTaxonomy

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<ncbi_taxonomy>

=cut

__PACKAGE__->table("ncbi_taxonomy");

=head1 ACCESSORS

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 species

  data_type: 'varchar'
  is_nullable: 0
  size: 100

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "species",
  { data_type => "varchar", is_nullable => 0, size => 100 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 0 },
);

=head1 PRIMARY KEY

=over 4

=item * L</ncbi_taxid>

=back

=cut

__PACKAGE__->set_primary_key("ncbi_taxid");

=head1 RELATIONS

=head2 pfama_ncbis

Type: has_many

Related object: L<PfamDB::PfamaNcbi>

=cut

__PACKAGE__->has_many(
  "pfama_ncbis",
  "PfamDB::PfamaNcbi",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
  undef,
);

=head2 pfamseq_antifams

Type: has_many

Related object: L<PfamDB::PfamseqAntifam>

=cut

__PACKAGE__->has_many(
  "pfamseq_antifams",
  "PfamDB::PfamseqAntifam",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
  undef,
);

=head2 pfamseq_ncbis

Type: has_many

Related object: L<PfamDB::PfamseqNcbi>

=cut

__PACKAGE__->has_many(
  "pfamseq_ncbis",
  "PfamDB::PfamseqNcbi",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
  undef,
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:PqV8TfxBscWGvQvYkllrbw


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
