use utf8;
package PfamLive::Result::ReleasedClanVersion;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::ReleasedClanVersion

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<released_clan_version>

=cut

__PACKAGE__->table("released_clan_version");

=head1 ACCESSORS

=head2 clan_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 6

=head2 desc_file

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=head2 version

  data_type: 'smallint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "clan_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 6 },
  "desc_file",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "version",
  { data_type => "smallint", is_nullable => 1 },
);

=head1 RELATIONS

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


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:WS1W/IQubgRNa8TFAESVfg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
