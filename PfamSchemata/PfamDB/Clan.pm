use utf8;
package PfamDB::Clan;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::Clan

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

=head2 clan_alignment_and_relationships

Type: has_many

Related object: L<PfamDB::ClanAlignmentAndRelationship>

=cut

__PACKAGE__->has_many(
  "clan_alignment_and_relationships",
  "PfamDB::ClanAlignmentAndRelationship",
  { "foreign.clan_acc" => "self.clan_acc" },
  undef,
);

=head2 clan_architectures

Type: has_many

Related object: L<PfamDB::ClanArchitecture>

=cut

__PACKAGE__->has_many(
  "clan_architectures",
  "PfamDB::ClanArchitecture",
  { "foreign.clan_acc" => "self.clan_acc" },
  undef,
);

=head2 clan_database_links

Type: has_many

Related object: L<PfamDB::ClanDatabaseLinks>

=cut

__PACKAGE__->has_many(
  "clan_database_links",
  "PfamDB::ClanDatabaseLinks",
  { "foreign.clan_acc" => "self.clan_acc" },
  undef,
);

=head2 clan_lit_refs

Type: has_many

Related object: L<PfamDB::ClanLitRef>

=cut

__PACKAGE__->has_many(
  "clan_lit_refs",
  "PfamDB::ClanLitRef",
  { "foreign.clan_acc" => "self.clan_acc" },
  undef,
);

=head2 clan_memberships

Type: has_many

Related object: L<PfamDB::ClanMembership>

=cut

__PACKAGE__->has_many(
  "clan_memberships",
  "PfamDB::ClanMembership",
  { "foreign.clan_acc" => "self.clan_acc" },
  undef,
);

=head2 clan_wikis

Type: has_many

Related object: L<PfamDB::ClanWiki>

=cut

__PACKAGE__->has_many(
  "clan_wikis",
  "PfamDB::ClanWiki",
  { "foreign.clan_acc" => "self.clan_acc" },
  undef,
);

=head2 released_clan_versions

Type: has_many

Related object: L<PfamDB::ReleasedClanVersion>

=cut

__PACKAGE__->has_many(
  "released_clan_versions",
  "PfamDB::ReleasedClanVersion",
  { "foreign.clan_acc" => "self.clan_acc" },
  undef,
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-09-08 12:51:14
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:+3bCetc4++qdScQLikBkmQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
