use utf8;
package PfamLive::Result::PfamAAuthor;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamAAuthor

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_author>

=cut

__PACKAGE__->table("pfamA_author");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 author_rank

  data_type: 'integer'
  is_nullable: 0

=head2 author_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "author_rank",
  { data_type => "integer", is_nullable => 0 },
  "author_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
);

=head1 UNIQUE CONSTRAINTS

=head2 C<acc_author>

=over 4

=item * L</pfama_acc>

=item * L</author_id>

=back

=cut

__PACKAGE__->add_unique_constraint("acc_author", ["pfama_acc", "author_id"]);

=head2 C<acc_rank>

=over 4

=item * L</pfama_acc>

=item * L</author_rank>

=back

=cut

__PACKAGE__->add_unique_constraint("acc_rank", ["pfama_acc", "author_rank"]);

=head1 RELATIONS

=head2 author

Type: belongs_to

Related object: L<PfamLive::Result::Author>

=cut

__PACKAGE__->belongs_to(
  "author",
  "PfamLive::Result::Author",
  { author_id => "author_id" },
  { is_deferrable => 1, on_delete => "RESTRICT", on_update => "RESTRICT" },
);

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamLive::Result::PfamA>

=cut

__PACKAGE__->belongs_to(
  "pfama_acc",
  "PfamLive::Result::PfamA",
  { pfama_acc => "pfama_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2018-04-04 11:46:42
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:fTOQ59M793orp/YxgJDO2Q

__PACKAGE__->add_unique_constraint("acc_rank", ["pfama_acc", "author_rank"]);
# SCP - non-ideal fix for allowing delete in FamilyIO.pm
__PACKAGE__->set_primary_key(__PACKAGE__->columns);


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
