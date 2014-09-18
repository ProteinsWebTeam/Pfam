use utf8;
package RfamDB::Result::Rfamseq;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::Rfamseq

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<rfamseq>

=cut

__PACKAGE__->table("rfamseq");

=head1 ACCESSORS

=head2 rfamseq_acc

  data_type: 'varchar'
  default_value: (empty string)
  is_nullable: 0
  size: 20

=head2 accession

  data_type: 'varchar'
  is_nullable: 0
  size: 15

=head2 version

  data_type: 'integer'
  is_nullable: 0

=head2 ncbi_id

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 mol_type

  data_type: 'enum'
  extra: {list => ["protein","genomic DNA","DNA","ss-DNA","RNA","genomic RNA","ds-RNA","ss-cRNA","ss-RNA","mRNA","tRNA","rRNA","snoRNA","snRNA","scRNA","pre-RNA","other RNA","other DNA","unassigned DNA","unassigned RNA","viral cRNA","cRNA","transcribed RNA"]}
  is_nullable: 0

=head2 length

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 1

=head2 description

  data_type: 'varchar'
  default_value: (empty string)
  is_nullable: 0
  size: 250

=head2 previous_acc

  data_type: 'mediumtext'
  is_nullable: 1

=head2 source

  data_type: 'char'
  is_nullable: 0
  size: 20

=cut

__PACKAGE__->add_columns(
  "rfamseq_acc",
  { data_type => "varchar", default_value => "", is_nullable => 0, size => 20 },
  "accession",
  { data_type => "varchar", is_nullable => 0, size => 15 },
  "version",
  { data_type => "integer", is_nullable => 0 },
  "ncbi_id",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "mol_type",
  {
    data_type => "enum",
    extra => {
      list => [
        "protein",
        "genomic DNA",
        "DNA",
        "ss-DNA",
        "RNA",
        "genomic RNA",
        "ds-RNA",
        "ss-cRNA",
        "ss-RNA",
        "mRNA",
        "tRNA",
        "rRNA",
        "snoRNA",
        "snRNA",
        "scRNA",
        "pre-RNA",
        "other RNA",
        "other DNA",
        "unassigned DNA",
        "unassigned RNA",
        "viral cRNA",
        "cRNA",
        "transcribed RNA",
      ],
    },
    is_nullable => 0,
  },
  "length",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 1,
  },
  "description",
  { data_type => "varchar", default_value => "", is_nullable => 0, size => 250 },
  "previous_acc",
  { data_type => "mediumtext", is_nullable => 1 },
  "source",
  { data_type => "char", is_nullable => 0, size => 20 },
);

=head1 PRIMARY KEY

=over 4

=item * L</rfamseq_acc>

=back

=cut

__PACKAGE__->set_primary_key("rfamseq_acc");

=head1 RELATIONS

=head2 features

Type: has_many

Related object: L<RfamDB::Result::Feature>

=cut

__PACKAGE__->has_many(
  "features",
  "RfamDB::Result::Feature",
  { "foreign.rfamseq_acc" => "self.rfamseq_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 full_regions

Type: has_many

Related object: L<RfamDB::Result::FullRegion>

=cut

__PACKAGE__->has_many(
  "full_regions",
  "RfamDB::Result::FullRegion",
  { "foreign.rfamseq_acc" => "self.rfamseq_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 ncbi

Type: belongs_to

Related object: L<RfamDB::Result::Taxonomy>

=cut

__PACKAGE__->belongs_to(
  "ncbi",
  "RfamDB::Result::Taxonomy",
  { ncbi_id => "ncbi_id" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 pdb_rfam_regs

Type: has_many

Related object: L<RfamDB::Result::PdbRfamReg>

=cut

__PACKAGE__->has_many(
  "pdb_rfam_regs",
  "RfamDB::Result::PdbRfamReg",
  { "foreign.rfamseq_acc" => "self.rfamseq_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 seed_regions

Type: has_many

Related object: L<RfamDB::Result::SeedRegion>

=cut

__PACKAGE__->has_many(
  "seed_regions",
  "RfamDB::Result::SeedRegion",
  { "foreign.rfamseq_acc" => "self.rfamseq_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-06-02 13:07:10
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:FSMZHxqtK0nnkkpPyrolag

# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
