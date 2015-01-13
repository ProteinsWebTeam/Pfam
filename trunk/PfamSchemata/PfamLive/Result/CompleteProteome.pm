use utf8;
package PfamLive::Result::CompleteProteome;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::CompleteProteome

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<complete_proteomes>

=cut

__PACKAGE__->table("complete_proteomes");

=head1 ACCESSORS

=head2 auto_proteome

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

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
  "auto_proteome",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
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

=head1 PRIMARY KEY

=over 4

=item * L</auto_proteome>

=back

=cut

__PACKAGE__->set_primary_key("auto_proteome");

=head1 UNIQUE CONSTRAINTS

=head2 C<ncbi_taxid>

=over 4

=item * L</ncbi_taxid>

=back

=cut

__PACKAGE__->add_unique_constraint("ncbi_taxid", ["ncbi_taxid"]);

=head1 RELATIONS

=head2 proteome_architectures

Type: has_many

Related object: L<PfamLive::Result::ProteomeArchitecture>

=cut

__PACKAGE__->has_many(
  "proteome_architectures",
  "PfamLive::Result::ProteomeArchitecture",
  { "foreign.auto_proteome" => "self.auto_proteome" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 proteome_pfamseqs

Type: has_many

Related object: L<PfamLive::Result::ProteomePfamseq>

=cut

__PACKAGE__->has_many(
  "proteome_pfamseqs",
  "PfamLive::Result::ProteomePfamseq",
  { "foreign.auto_proteome" => "self.auto_proteome" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 proteome_regions_auto_proteomes

Type: has_many

Related object: L<PfamLive::Result::ProteomeRegion>

=cut

__PACKAGE__->has_many(
  "proteome_regions_auto_proteomes",
  "PfamLive::Result::ProteomeRegion",
  { "foreign.auto_proteome" => "self.auto_proteome" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 proteome_regions_auto_proteomes_2s

Type: has_many

Related object: L<PfamLive::Result::ProteomeRegion>

=cut

__PACKAGE__->has_many(
  "proteome_regions_auto_proteomes_2s",
  "PfamLive::Result::ProteomeRegion",
  { "foreign.auto_proteome" => "self.auto_proteome" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:swnUNBf+gy+GCzjqWuVYtQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
