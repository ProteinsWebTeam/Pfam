use utf8;
package RfamLive::Result::Lock;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::Lock

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<_lock>

=cut

__PACKAGE__->table("_lock");

=head1 ACCESSORS

=head2 locker

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=head2 rfam_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 40

Do you lock individual families? Do ever lock the whole database?

=cut

__PACKAGE__->add_columns(
  "locker",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "rfam_acc",
  { data_type => "varchar", is_nullable => 0, size => 40 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:2CB4KOqmBKhhxGFryyaWqQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
