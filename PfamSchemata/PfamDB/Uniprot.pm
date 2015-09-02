use utf8;
package PfamDB::Uniprot;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::Uniprot

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<uniprot>

=cut

__PACKAGE__->table("uniprot");

=head1 ACCESSORS

=head2 uniprot_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=head2 uniprot_id

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 seq_version

  data_type: 'tinyint'
  is_nullable: 0

=head2 crc64

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 md5

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=head2 description

  data_type: 'text'
  is_nullable: 0

=head2 evidence

  data_type: 'tinyint'
  is_foreign_key: 1
  is_nullable: 0

=head2 length

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 species

  data_type: 'text'
  is_nullable: 0

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 1

=head2 is_fragment

  data_type: 'tinyint'
  is_nullable: 1

=head2 sequence

  accessor: undef
  data_type: 'blob'
  is_nullable: 0

=head2 updated

  data_type: 'timestamp'
  datetime_undef_if_invalid: 1
  default_value: current_timestamp
  is_nullable: 0

=head2 created

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 1

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 ref_proteome

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 1

=head2 complete_proteome

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "uniprot_acc",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "uniprot_id",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "seq_version",
  { data_type => "tinyint", is_nullable => 0 },
  "crc64",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "md5",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "description",
  { data_type => "text", is_nullable => 0 },
  "evidence",
  { data_type => "tinyint", is_foreign_key => 1, is_nullable => 0 },
  "length",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "species",
  { data_type => "text", is_nullable => 0 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 1 },
  "is_fragment",
  { data_type => "tinyint", is_nullable => 1 },
  "sequence",
  { accessor => undef, data_type => "blob", is_nullable => 0 },
  "updated",
  {
    data_type => "timestamp",
    datetime_undef_if_invalid => 1,
    default_value => \"current_timestamp",
    is_nullable => 0,
  },
  "created",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 1,
  },
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "ref_proteome",
  { data_type => "tinyint", default_value => 0, is_nullable => 1 },
  "complete_proteome",
  { data_type => "tinyint", default_value => 0, is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</uniprot_acc>

=back

=cut

__PACKAGE__->set_primary_key("uniprot_acc");

=head1 RELATIONS

=head2 evidence

Type: belongs_to

Related object: L<PfamDB::Evidence>

=cut

__PACKAGE__->belongs_to("evidence", "PfamDB::Evidence", { evidence => "evidence" });

=head2 pdb_residue_datas

Type: has_many

Related object: L<PfamDB::PdbResidueData>

=cut

__PACKAGE__->has_many(
  "pdb_residue_datas",
  "PfamDB::PdbResidueData",
  { "foreign.pfamseq_acc" => "self.uniprot_acc" },
  undef,
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-09-02 16:48:11
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:IDfV1mwBCF/gYBviGfofrA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
