use utf8;
package PfamLive::Result::TmpPfamseq;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::TmpPfamseq

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<tmp_pfamseq>

=cut

__PACKAGE__->table("tmp_pfamseq");

=head1 ACCESSORS

=head2 pfamseq_id

  data_type: 'varchar'
  is_nullable: 0
  size: 12

=head2 pfamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 10

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
  is_nullable: 1

=head2 genome_seq

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 1

=head2 auto_architecture

  data_type: 'integer'
  is_nullable: 1

=head2 treefam_acc

  data_type: 'varchar'
  is_nullable: 1
  size: 8

=cut

__PACKAGE__->add_columns(
  "pfamseq_id",
  { data_type => "varchar", is_nullable => 0, size => 12 },
  "pfamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "seq_version",
  { data_type => "tinyint", is_nullable => 0 },
  "crc64",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "md5",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "description",
  { data_type => "text", is_nullable => 0 },
  "evidence",
  { data_type => "tinyint", is_nullable => 0 },
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
    is_nullable => 1,
  },
  "genome_seq",
  { data_type => "tinyint", default_value => 0, is_nullable => 1 },
  "auto_architecture",
  { data_type => "integer", is_nullable => 1 },
  "treefam_acc",
  { data_type => "varchar", is_nullable => 1, size => 8 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pfamseq_acc>

=back

=cut

__PACKAGE__->set_primary_key("pfamseq_acc");


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:9YRMeRTIqcW2FJt2MXGDjw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
