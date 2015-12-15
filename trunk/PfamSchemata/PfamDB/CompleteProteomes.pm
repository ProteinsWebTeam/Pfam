use utf8;
package PfamDB::CompleteProteomes;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::CompleteProteomes

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<complete_proteomes>

=cut

__PACKAGE__->table("complete_proteomes");

=head1 ACCESSORS

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 species

  data_type: 'varchar'
  is_nullable: 1
  size: 256

=head2 grouping

  data_type: 'varchar'
  is_nullable: 1
  size: 20

=head2 num_distinct_regions

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 num_total_regions

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 num_proteins

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 sequence_coverage

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 residue_coverage

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 total_genome_proteins

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 total_aa_length

  data_type: 'bigint'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 total_aa_covered

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 1

=head2 total_seqs_covered

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 1

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
  { data_type => "varchar", is_nullable => 1, size => 256 },
  "grouping",
  { data_type => "varchar", is_nullable => 1, size => 20 },
  "num_distinct_regions",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "num_total_regions",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "num_proteins",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "sequence_coverage",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "residue_coverage",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "total_genome_proteins",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "total_aa_length",
  {
    data_type => "bigint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "total_aa_covered",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 1,
  },
  "total_seqs_covered",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 1,
  },
);

=head1 UNIQUE CONSTRAINTS

=head2 C<ncbi_taxid>

=over 4

=item * L</ncbi_taxid>

=back

=cut

__PACKAGE__->add_unique_constraint("ncbi_taxid", ["ncbi_taxid"]);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-12-02 12:30:50
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Vc/ir6Ox3xT+4393ZGhRBg

__PACKAGE__->has_one(
  "ncbi_taxid_data",
  "PfamDB::Taxonomy",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
  undef,
);

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
