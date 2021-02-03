use utf8;
package PfamLive::Result::DeadFamily;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::DeadFamily

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<dead_family>

=cut

__PACKAGE__->table("dead_family");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 pfama_id

  data_type: 'varchar'
  is_nullable: 0
  size: 40

=head2 comment

  data_type: 'mediumtext'
  is_nullable: 1

=head2 forward_to

  data_type: 'varchar'
  is_nullable: 1
  size: 7

=head2 user

  data_type: 'varchar'
  default_value: 'anon'
  is_nullable: 0
  size: 20

=head2 killed

  data_type: 'timestamp'
  datetime_undef_if_invalid: 1
  default_value: current_timestamp
  is_nullable: 0

=head2 title

  data_type: 'tinytext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "pfama_id",
  { data_type => "varchar", is_nullable => 0, size => 40 },
  "comment",
  { data_type => "mediumtext", is_nullable => 1 },
  "forward_to",
  { data_type => "varchar", is_nullable => 1, size => 7 },
  "user",
  {
    data_type => "varchar",
    default_value => "anon",
    is_nullable => 0,
    size => 20,
  },
  "killed",
  {
    data_type => "timestamp",
    datetime_undef_if_invalid => 1,
    default_value => \"current_timestamp",
    is_nullable => 0,
  },
  "title",
  { data_type => "tinytext", is_nullable => 1 },
);

=head1 UNIQUE CONSTRAINTS

=head2 C<pfamA_acc>

=over 4

=item * L</pfama_acc>

=back

=cut

__PACKAGE__->add_unique_constraint("pfamA_acc", ["pfama_acc"]);


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2020-12-16 13:56:32
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:uFXsPPbWxsn9+q0abmKGZA


# You can replace this text with custom code or comments, and it will be preserved on regeneration

=head2 pfam_a_wikis

Type: has_many

Related object: L<PfamLive::Result::PfamAWiki>

=cut

__PACKAGE__->has_many(
  "pfam_a_wikis",
  "PfamLive::Result::PfamAWiki",
  { "foreign.pfamA_acc" => "self.pfamA_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


1;
