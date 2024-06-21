use utf8;
package PfamLive::Result::Clan;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Clan

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
  size: 6

=head2 clan_id

  data_type: 'varchar'
  is_nullable: 0
  size: 40

=head2 previous_id

  data_type: 'varchar'
  is_nullable: 1
  size: 75

=head2 clan_description

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 clan_author

  data_type: 'tinytext'
  is_nullable: 1

=head2 deposited_by

  data_type: 'varchar'
  default_value: 'anon'
  is_nullable: 0
  size: 100

=head2 clan_comment

  data_type: 'longtext'
  is_nullable: 1

=head2 updated

  data_type: 'timestamp'
  datetime_undef_if_invalid: 1
  default_value: current_timestamp
  is_nullable: 0

=head2 created

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 1

=head2 version

  data_type: 'smallint'
  is_nullable: 1

=head2 number_structures

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_archs

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_species

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_sequences

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 competed

  data_type: 'tinyint'
  is_nullable: 1

=head2 uniprot_competed

  data_type: 'tinyint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "clan_acc",
  { data_type => "varchar", is_nullable => 0, size => 6 },
  "clan_id",
  { data_type => "varchar", is_nullable => 0, size => 40 },
  "previous_id",
  { data_type => "varchar", is_nullable => 1, size => 75 },
  "clan_description",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "clan_author",
  { data_type => "tinytext", is_nullable => 1 },
  "deposited_by",
  {
    data_type => "varchar",
    default_value => "anon",
    is_nullable => 0,
    size => 100,
  },
  "clan_comment",
  { data_type => "longtext", is_nullable => 1 },
  "updated",
  {
    data_type => "timestamp",
    datetime_undef_if_invalid => 1,
    default_value => \"current_timestamp",
    is_nullable => 0,
  },
  "created",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 1,
  },
  "version",
  { data_type => "smallint", is_nullable => 1 },
  "number_structures",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "number_archs",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "number_species",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "number_sequences",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "competed",
  { data_type => "tinyint", is_nullable => 1 },
  "uniprot_competed",
  { data_type => "tinyint", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</clan_acc>

=back

=cut

__PACKAGE__->set_primary_key("clan_acc");

=head1 UNIQUE CONSTRAINTS

=head2 C<clan_id>

=over 4

=item * L</clan_id>

=back

=cut

__PACKAGE__->add_unique_constraint("clan_id", ["clan_id"]);

=head1 RELATIONS

=head2 clan_alignments_and_relationship

Type: has_many

Related object: L<PfamLive::Result::ClanAlignmentAndRelationship>

=cut

__PACKAGE__->has_many(
  "clan_alignments_and_relationship",
  "PfamLive::Result::ClanAlignmentAndRelationship",
  { "foreign.clan_acc" => "self.clan_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 clan_architectures

Type: has_many

Related object: L<PfamLive::Result::ClanArchitecture>

=cut

__PACKAGE__->has_many(
  "clan_architectures",
  "PfamLive::Result::ClanArchitecture",
  { "foreign.clan_acc" => "self.clan_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 clan_wikis

Type: has_many

Related object: L<PfamLive::Result::PfamAWiki>

=cut

__PACKAGE__->has_many(
  "clan_wikis",
  "PfamLive::Result::ClanWiki",
  { "foreign.clan_acc" => "self.clan_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 clan_database_links

Type: has_many

Related object: L<PfamLive::Result::ClanDatabaseLink>

=cut

__PACKAGE__->has_many(
  "clan_database_links",
  "PfamLive::Result::ClanDatabaseLink",
  { "foreign.clan_acc" => "self.clan_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 clan_lit_refs

Type: has_many

Related object: L<PfamLive::Result::ClanLitRef>

=cut

__PACKAGE__->has_many(
  "clan_lit_refs",
  "PfamLive::Result::ClanLitRef",
  { "foreign.clan_acc" => "self.clan_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 clan_memberships

Type: has_many

Related object: L<PfamLive::Result::ClanMembership>

=cut

__PACKAGE__->has_many(
  "clan_memberships",
  "PfamLive::Result::ClanMembership",
  { "foreign.clan_acc" => "self.clan_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 released_clan_versions

Type: has_many

Related object: L<PfamLive::Result::ReleasedClanVersion>

=cut

__PACKAGE__->has_many(
  "released_clan_versions",
  "PfamLive::Result::ReleasedClanVersion",
  { "foreign.clan_acc" => "self.clan_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2016-05-17 16:00:13
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Ah7HIjfhllbLB813wAW5IQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
