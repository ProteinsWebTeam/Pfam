use utf8;
package PfamLive::Result::ClanArchitecture;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::ClanArchitecture

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<clan_architecture>

=cut

__PACKAGE__->table("clan_architecture");

=head1 ACCESSORS

=head2 clan_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 6

=head2 auto_architecture

  data_type: 'bigint'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "clan_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 6 },
  "auto_architecture",
  {
    data_type => "bigint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
);

=head1 RELATIONS

=head2 auto_architecture

Type: belongs_to

Related object: L<PfamLive::Result::Architecture>

=cut

__PACKAGE__->belongs_to(
  "auto_architecture",
  "PfamLive::Result::Architecture",
  { auto_architecture => "auto_architecture" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 clan_acc

Type: belongs_to

Related object: L<PfamLive::Result::Clan>

=cut

__PACKAGE__->belongs_to(
  "clan_acc",
  "PfamLive::Result::Clan",
  { clan_acc => "clan_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-12-02 12:31:25
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:oQlukbUa5XMbEYxbMYP2PA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
