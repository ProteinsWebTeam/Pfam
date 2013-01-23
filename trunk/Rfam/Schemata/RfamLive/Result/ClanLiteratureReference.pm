use utf8;
package RfamLive::Result::ClanLiteratureReference;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::ClanLiteratureReference

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<clan_literature_references>

=cut

__PACKAGE__->table("clan_literature_references");

=head1 ACCESSORS

=head2 clan_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 auto_lit

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 0

=head2 comment

  data_type: 'tinytext'
  is_nullable: 1

=head2 order_added

  data_type: 'tinyint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "clan_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "auto_lit",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
  "comment",
  { data_type => "tinytext", is_nullable => 1 },
  "order_added",
  { data_type => "tinyint", is_nullable => 1 },
);

=head1 RELATIONS

=head2 auto_lit

Type: belongs_to

Related object: L<RfamLive::Result::LiteratureReference>

=cut

__PACKAGE__->belongs_to(
  "auto_lit",
  "RfamLive::Result::LiteratureReference",
  { auto_lit => "auto_lit" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 clan_acc

Type: belongs_to

Related object: L<RfamLive::Result::Clan>

=cut

__PACKAGE__->belongs_to(
  "clan_acc",
  "RfamLive::Result::Clan",
  { clan_acc => "clan_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:O+QWb4USG8XZEPN0eiA60Q


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
