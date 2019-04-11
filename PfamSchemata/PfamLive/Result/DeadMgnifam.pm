use utf8;
package PfamLive::Result::DeadMgnifam;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::DeadMgnifam

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<dead_mgnifam>

=cut

__PACKAGE__->table("dead_mgnifam");

=head1 ACCESSORS

=head2 mgnify_acc

  data_type: 'varchar'
  is_nullable: 1
  size: 9

=head2 mgnify_id

  data_type: 'varchar'
  is_nullable: 0
  size: 40

=head2 comment

  data_type: 'mediumtext'
  is_nullable: 1

=head2 forward_to

  data_type: 'varchar'
  is_nullable: 1
  size: 7

=head2 user

  data_type: 'varchar'
  default_value: 'anon'
  is_nullable: 0
  size: 10

=head2 killed

  data_type: 'timestamp'
  datetime_undef_if_invalid: 1
  default_value: current_timestamp
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "mgnify_acc",
  { data_type => "varchar", is_nullable => 1, size => 9 },
  "mgnify_id",
  { data_type => "varchar", is_nullable => 0, size => 40 },
  "comment",
  { data_type => "mediumtext", is_nullable => 1 },
  "forward_to",
  { data_type => "varchar", is_nullable => 1, size => 7 },
  "user",
  {
    data_type => "varchar",
    default_value => "anon",
    is_nullable => 0,
    size => 10,
  },
  "killed",
  {
    data_type => "timestamp",
    datetime_undef_if_invalid => 1,
    default_value => \"current_timestamp",
    is_nullable => 0,
  },
);

=head1 UNIQUE CONSTRAINTS

=head2 C<mgnify_acc>

=over 4

=item * L</mgnify_acc>

=back

=cut

__PACKAGE__->add_unique_constraint("mgnify_acc", ["mgnify_acc"]);


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2019-04-11 09:45:24
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:i3oNUeKmQAqM1V49CCAekQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
