use utf8;
package RfamLive::Result::PostProcess;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::PostProcess

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<_post_process>

=cut

__PACKAGE__->table("_post_process");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 author

  data_type: 'varchar'
  is_nullable: 0
  size: 45

=head2 uuid

  data_type: 'varchar'
  is_nullable: 0
  size: 45

=head2 status

  data_type: 'enum'
  extra: {list => ["PEND","RUN","DONE","FAIL","KILL"]}
  is_nullable: 0

=head2 created

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 0

=head2 opened

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 1

=head2 closed

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 1

=head2 message

  data_type: 'mediumtext'
  is_nullable: 1

=head2 lsf_id

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "author",
  { data_type => "varchar", is_nullable => 0, size => 45 },
  "uuid",
  { data_type => "varchar", is_nullable => 0, size => 45 },
  "status",
  {
    data_type => "enum",
    extra => { list => ["PEND", "RUN", "DONE", "FAIL", "KILL"] },
    is_nullable => 0,
  },
  "created",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 0,
  },
  "opened",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 1,
  },
  "closed",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 1,
  },
  "message",
  { data_type => "mediumtext", is_nullable => 1 },
  "lsf_id",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
);

=head1 RELATIONS

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


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-31 10:52:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Q1w5kc21RiaIunHSFxzKvA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
