use utf8;
package PfamLive::Result::ClanLitRef;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::ClanLitRef

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<clan_lit_ref>

=cut

__PACKAGE__->table("clan_lit_ref");

=head1 ACCESSORS

=head2 clan_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 6

=head2 auto_lit

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 order_added

  data_type: 'tinyint'
  is_nullable: 0

=head2 comment

  data_type: 'tinytext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "clan_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 6 },
  "auto_lit",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "order_added",
  { data_type => "tinyint", is_nullable => 0 },
  "comment",
  { data_type => "tinytext", is_nullable => 1 },
);

=head1 RELATIONS

=head2 auto_lit

Type: belongs_to

Related object: L<PfamLive::Result::LiteratureReference>

=cut

__PACKAGE__->belongs_to(
  "auto_lit",
  "PfamLive::Result::LiteratureReference",
  { auto_lit => "auto_lit" },
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


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:CqjATzFocunB9mePUEOydg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
