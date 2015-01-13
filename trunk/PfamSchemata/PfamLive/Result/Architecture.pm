use utf8;
package PfamLive::Result::Architecture;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Architecture

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<architecture>

=cut

__PACKAGE__->table("architecture");

=head1 ACCESSORS

=head2 auto_architecture

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 architecture

  data_type: 'text'
  is_nullable: 1

=head2 type_example

  data_type: 'varchar'
  default_value: 0
  is_nullable: 0
  size: 10

=head2 no_seqs

  data_type: 'integer'
  default_value: 0
  is_nullable: 0

=head2 architecture_acc

  data_type: 'text'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_architecture",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "architecture",
  { data_type => "text", is_nullable => 1 },
  "type_example",
  { data_type => "varchar", default_value => 0, is_nullable => 0, size => 10 },
  "no_seqs",
  { data_type => "integer", default_value => 0, is_nullable => 0 },
  "architecture_acc",
  { data_type => "text", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_architecture>

=back

=cut

__PACKAGE__->set_primary_key("auto_architecture");

=head1 RELATIONS

=head2 clan_architectures

Type: has_many

Related object: L<PfamLive::Result::ClanArchitecture>

=cut

__PACKAGE__->has_many(
  "clan_architectures",
  "PfamLive::Result::ClanArchitecture",
  { "foreign.auto_architecture" => "self.auto_architecture" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_architectures

Type: has_many

Related object: L<PfamLive::Result::PfamAArchitecture>

=cut

__PACKAGE__->has_many(
  "pfam_a_architectures",
  "PfamLive::Result::PfamAArchitecture",
  { "foreign.auto_architecture" => "self.auto_architecture" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 proteome_architectures

Type: has_many

Related object: L<PfamLive::Result::ProteomeArchitecture>

=cut

__PACKAGE__->has_many(
  "proteome_architectures",
  "PfamLive::Result::ProteomeArchitecture",
  { "foreign.auto_architecture" => "self.auto_architecture" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:+2NlFNk0dO0xHmlog1Bs/Q


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
