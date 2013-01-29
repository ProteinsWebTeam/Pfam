use utf8;
package RfamLive::Result::Feature;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::Feature

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<features>

=cut

__PACKAGE__->table("features");

=head1 ACCESSORS

=head2 rfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 14

=head2 database_id

  data_type: 'varchar'
  is_nullable: 0
  size: 50

=head2 primary_id

  data_type: 'varchar'
  is_nullable: 0
  size: 100

=head2 secondary_id

  data_type: 'varchar'
  is_nullable: 1
  size: 255

=head2 feat_orient

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 0

=head2 feat_start

  data_type: 'bigint'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 feat_end

  data_type: 'bigint'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 quaternary_id

  data_type: 'varchar'
  is_nullable: 1
  size: 150

=cut

__PACKAGE__->add_columns(
  "rfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 14 },
  "database_id",
  { data_type => "varchar", is_nullable => 0, size => 50 },
  "primary_id",
  { data_type => "varchar", is_nullable => 0, size => 100 },
  "secondary_id",
  { data_type => "varchar", is_nullable => 1, size => 255 },
  "feat_orient",
  { data_type => "tinyint", default_value => 0, is_nullable => 0 },
  "feat_start",
  {
    data_type => "bigint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "feat_end",
  {
    data_type => "bigint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "quaternary_id",
  { data_type => "varchar", is_nullable => 1, size => 150 },
);

=head1 RELATIONS

=head2 rfamseq_acc

Type: belongs_to

Related object: L<RfamLive::Result::Rfamseq>

=cut

__PACKAGE__->belongs_to(
  "rfamseq_acc",
  "RfamLive::Result::Rfamseq",
  { rfamseq_acc => "rfamseq_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Kc02A7YkUokpLALtShHGkg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
