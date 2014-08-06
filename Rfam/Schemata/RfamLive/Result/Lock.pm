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

=head2 locked

  data_type: 'tinyint'
  is_nullable: 0

=head2 locker

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=head2 allowcommits

  data_type: 'tinyint'
  is_nullable: 0

Do you lock individual families? Do ever lock the whole database?

=head2 alsoallow

  data_type: 'text'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "locked",
  { data_type => "tinyint", is_nullable => 0 },
  "locker",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "allowcommits",
  { data_type => "tinyint", is_nullable => 0 },
  "alsoallow",
  { data_type => "text", is_nullable => 1 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:NRup1ddWOT3cVS844fOViA

__PACKAGE__->set_primary_key('locked');

1;
