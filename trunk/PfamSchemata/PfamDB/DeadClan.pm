use utf8;
package PfamDB::DeadClan;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::DeadClan

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<dead_clan>

=cut

__PACKAGE__->table("dead_clan");

=head1 ACCESSORS

=head2 clan_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 clan_id

  data_type: 'varchar'
  is_nullable: 0
  size: 40

=head2 clan_description

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 clan_membership

  data_type: 'longtext'
  is_nullable: 1

=head2 comment

  data_type: 'mediumtext'
  is_nullable: 1

=head2 forward_to

  data_type: 'varchar'
  is_nullable: 1
  size: 6

=head2 user

  data_type: 'tinytext'
  is_nullable: 1

=head2 killed

  data_type: 'timestamp'
  datetime_undef_if_invalid: 1
  default_value: current_timestamp
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "clan_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "clan_id",
  { data_type => "varchar", is_nullable => 0, size => 40 },
  "clan_description",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "clan_membership",
  { data_type => "longtext", is_nullable => 1 },
  "comment",
  { data_type => "mediumtext", is_nullable => 1 },
  "forward_to",
  { data_type => "varchar", is_nullable => 1, size => 6 },
  "user",
  { data_type => "tinytext", is_nullable => 1 },
  "killed",
  {
    data_type => "timestamp",
    datetime_undef_if_invalid => 1,
    default_value => \"current_timestamp",
    is_nullable => 0,
  },
);

=head1 UNIQUE CONSTRAINTS

=head2 C<clan_acc>

=over 4

=item * L</clan_acc>

=back

=cut

__PACKAGE__->add_unique_constraint("clan_acc", ["clan_acc"]);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:6BRMUv5QaFxz6wxJ3IZDyw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
