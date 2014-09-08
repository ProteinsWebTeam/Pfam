use utf8;
package PfamLive::Result::NcbiTaxonomy;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::NcbiTaxonomy

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

=head2 pfam_a_ncbis

Type: has_many

Related object: L<PfamLive::Result::PfamANcbi>

=cut

__PACKAGE__->has_many(
  "pfam_a_ncbis",
  "PfamLive::Result::PfamANcbi",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfamseq_antifams

Type: has_many

Related object: L<PfamLive::Result::PfamseqAntifam>

=cut

__PACKAGE__->has_many(
  "pfamseq_antifams",
  "PfamLive::Result::PfamseqAntifam",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfamseq_ncbis

Type: has_many

Related object: L<PfamLive::Result::PfamseqNcbi>

=cut

__PACKAGE__->has_many(
  "pfamseq_ncbis",
  "PfamLive::Result::PfamseqNcbi",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfamseqs

Type: has_many

Related object: L<PfamLive::Result::Pfamseq>

=cut

__PACKAGE__->has_many(
  "pfamseqs",
  "PfamLive::Result::Pfamseq",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:pT8CzpOCNkoSrHDwF95m4A


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
