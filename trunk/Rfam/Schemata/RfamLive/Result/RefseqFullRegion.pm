use utf8;
package RfamLive::Result::RefseqFullRegion;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::RefseqFullRegion

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<refseq_full_region>

=cut

__PACKAGE__->table("refseq_full_region");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 refseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 14

=head2 seq_start

  data_type: 'bigint'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 seq_end

  data_type: 'bigint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 bit_score

  data_type: 'double precision'
  default_value: 0.00
  is_nullable: 0
  size: [7,2]

99999.99 is the approx limit from Infernal.

=head2 evalue_score

  data_type: 'varchar'
  default_value: 0
  is_nullable: 0
  size: 15

=head2 cm_start

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 cm_end

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 truncated

  data_type: 'enum'
  extra: {list => [0,5,3,53]}
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "refseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 14 },
  "seq_start",
  {
    data_type => "bigint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "seq_end",
  { data_type => "bigint", extra => { unsigned => 1 }, is_nullable => 0 },
  "bit_score",
  {
    data_type => "double precision",
    default_value => "0.00",
    is_nullable => 0,
    size => [7, 2],
  },
  "evalue_score",
  { data_type => "varchar", default_value => 0, is_nullable => 0, size => 15 },
  "cm_start",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 0 },
  "cm_end",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 0 },
  "truncated",
  {
    data_type => "enum",
    extra => { list => [0, 5, 3, 53] },
    is_nullable => 0,
  },
);

=head1 RELATIONS

=head2 refseq_acc

Type: belongs_to

Related object: L<RfamLive::Result::Refseq>

=cut

__PACKAGE__->belongs_to(
  "refseq_acc",
  "RfamLive::Result::Refseq",
  { refseq_acc => "refseq_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 rfam_acc

Type: belongs_to

Related object: L<RfamLive::Result::Family>

=cut

__PACKAGE__->belongs_to(
  "rfam_acc",
  "RfamLive::Result::Family",
  { rfam_acc => "rfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:1gfEVnL9bEGlItykYJAQCA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
