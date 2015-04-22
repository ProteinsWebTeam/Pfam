use utf8;
package PfamDB::ReleasedClanVersion;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::ReleasedClanVersion

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

Related object: L<PfamDB::Clan>

=cut

__PACKAGE__->belongs_to("clan_acc", "PfamDB::Clan", { clan_acc => "clan_acc" });


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:FiRbY2HFrFN4rAymAWte/Q


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
