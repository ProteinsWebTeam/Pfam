use utf8;
package RfamLive::Result::Clan;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::Clan

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<clan>

=cut

__PACKAGE__->table("clan");

=head1 ACCESSORS

=head2 clan_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 id

  data_type: 'varchar'
  is_nullable: 1
  size: 40

=head2 description

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 author

  data_type: 'tinytext'
  is_nullable: 1

=head2 comment

  data_type: 'longtext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "clan_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "id",
  { data_type => "varchar", is_nullable => 1, size => 40 },
  "description",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "author",
  { data_type => "tinytext", is_nullable => 1 },
  "comment",
  { data_type => "longtext", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</clan_acc>

=back

=cut

__PACKAGE__->set_primary_key("clan_acc");

=head1 RELATIONS

=head2 clan_database_links

Type: has_many

Related object: L<RfamLive::Result::ClanDatabaseLink>

=cut

__PACKAGE__->has_many(
  "clan_database_links",
  "RfamLive::Result::ClanDatabaseLink",
  { "foreign.clan_acc" => "self.clan_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 clan_literature_references

Type: has_many

Related object: L<RfamLive::Result::ClanLiteratureReference>

=cut

__PACKAGE__->has_many(
  "clan_literature_references",
  "RfamLive::Result::ClanLiteratureReference",
  { "foreign.clan_acc" => "self.clan_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 clan_memberships

Type: has_many

Related object: L<RfamLive::Result::ClanMembership>

=cut

__PACKAGE__->has_many(
  "clan_memberships",
  "RfamLive::Result::ClanMembership",
  { "foreign.clan_acc" => "self.clan_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:qlq27f67cV5BRqBUoKSVug


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
