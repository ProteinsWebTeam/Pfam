use utf8;
package PfamLive::Result::MgnifamRegFull;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::MgnifamRegFull

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<mgnifam_reg_full>

=cut

__PACKAGE__->table("mgnifam_reg_full");

=head1 ACCESSORS

=head2 auto_mgnifam_reg_full

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 mgnifam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 9

=head2 mgnifamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 seq_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 seq_end

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 ali_start

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 ali_end

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 model_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 model_end

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 domain_bits_score

  data_type: 'double precision'
  default_value: 0.00
  is_nullable: 0
  size: [8,2]

=head2 domain_evalue_score

  data_type: 'varchar'
  is_nullable: 0
  size: 15

=head2 sequence_bits_score

  data_type: 'double precision'
  default_value: 0.00
  is_nullable: 0
  size: [8,2]

=head2 sequence_evalue_score

  data_type: 'varchar'
  is_nullable: 0
  size: 15

=cut

__PACKAGE__->add_columns(
  "auto_mgnifam_reg_full",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "mgnifam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 9 },
  "mgnifamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "seq_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "seq_end",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "ali_start",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 0 },
  "ali_end",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 0 },
  "model_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "model_end",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "domain_bits_score",
  {
    data_type => "double precision",
    default_value => "0.00",
    is_nullable => 0,
    size => [8, 2],
  },
  "domain_evalue_score",
  { data_type => "varchar", is_nullable => 0, size => 15 },
  "sequence_bits_score",
  {
    data_type => "double precision",
    default_value => "0.00",
    is_nullable => 0,
    size => [8, 2],
  },
  "sequence_evalue_score",
  { data_type => "varchar", is_nullable => 0, size => 15 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_mgnifam_reg_full>

=back

=cut

__PACKAGE__->set_primary_key("auto_mgnifam_reg_full");

=head1 RELATIONS

=head2 mgnifam_acc

Type: belongs_to

Related object: L<PfamLive::Result::Mgnifam>

=cut

__PACKAGE__->belongs_to(
  "mgnifam_acc",
  "PfamLive::Result::Mgnifam",
  { mgnifam_acc => "mgnifam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2019-05-03 13:41:53
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:QW2BP1+1smWH9kbUEPuekg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
