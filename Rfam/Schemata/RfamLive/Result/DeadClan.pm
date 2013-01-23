use utf8;
package RfamLive::Result::DeadClan;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::DeadClan

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
  default_value: (empty string)
  is_nullable: 0
  size: 7

=head2 clan_id

  data_type: 'varchar'
  is_nullable: 0
  size: 40

Added. Add author?

=head2 comment

  data_type: 'mediumtext'
  is_nullable: 1

=head2 forward_to

  data_type: 'varchar'
  is_nullable: 1
  size: 7

=head2 title

  data_type: 'varchar'
  is_nullable: 1
  size: 150

=cut

__PACKAGE__->add_columns(
  "clan_acc",
  { data_type => "varchar", default_value => "", is_nullable => 0, size => 7 },
  "clan_id",
  { data_type => "varchar", is_nullable => 0, size => 40 },
  "comment",
  { data_type => "mediumtext", is_nullable => 1 },
  "forward_to",
  { data_type => "varchar", is_nullable => 1, size => 7 },
  "title",
  { data_type => "varchar", is_nullable => 1, size => 150 },
);

=head1 UNIQUE CONSTRAINTS

=head2 C<rfam_acc>

=over 4

=item * L</clan_acc>

=back

=cut

__PACKAGE__->add_unique_constraint("rfam_acc", ["clan_acc"]);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:tMeVuWPgETUliHC2M2S38Q


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
