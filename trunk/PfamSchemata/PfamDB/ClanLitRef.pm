use utf8;
package PfamDB::ClanLitRef;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::ClanLitRef

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

Related object: L<PfamDB::LiteratureReference>

=cut

__PACKAGE__->belongs_to(
  "auto_lit",
  "PfamDB::LiteratureReference",
  { auto_lit => "auto_lit" },
);

=head2 clan_acc

Type: belongs_to

Related object: L<PfamDB::Clan>

=cut

__PACKAGE__->belongs_to("clan_acc", "PfamDB::Clan", { clan_acc => "clan_acc" });


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ftgaAhA3mBOwzuNnA2aTxw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
