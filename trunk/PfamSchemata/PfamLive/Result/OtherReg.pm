use utf8;
package PfamLive::Result::OtherReg;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::OtherReg

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<other_reg>

=cut

__PACKAGE__->table("other_reg");

=head1 ACCESSORS

=head2 region_id

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 seq_start

  data_type: 'mediumint'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 seq_end

  data_type: 'mediumint'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 type_id

  data_type: 'varchar'
  is_nullable: 0
  size: 20

=head2 source_id

  data_type: 'varchar'
  is_nullable: 0
  size: 20

=head2 score

  data_type: 'double precision'
  is_nullable: 1
  size: [16,4]

=head2 orientation

  data_type: 'varchar'
  is_nullable: 1
  size: 4

=cut

__PACKAGE__->add_columns(
  "region_id",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "seq_start",
  {
    data_type => "mediumint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "seq_end",
  {
    data_type => "mediumint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "type_id",
  { data_type => "varchar", is_nullable => 0, size => 20 },
  "source_id",
  { data_type => "varchar", is_nullable => 0, size => 20 },
  "score",
  { data_type => "double precision", is_nullable => 1, size => [16, 4] },
  "orientation",
  { data_type => "varchar", is_nullable => 1, size => 4 },
);

=head1 PRIMARY KEY

=over 4

=item * L</region_id>

=back

=cut

__PACKAGE__->set_primary_key("region_id");

=head1 RELATIONS

=head2 pfamseq_acc

Type: belongs_to

Related object: L<PfamLive::Result::Pfamseq>

=cut

__PACKAGE__->belongs_to(
  "pfamseq_acc",
  "PfamLive::Result::Pfamseq",
  { pfamseq_acc => "pfamseq_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:TnmXAnBO7KZB8FZCiepFTw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
