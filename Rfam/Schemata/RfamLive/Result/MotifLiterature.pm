use utf8;
package RfamLive::Result::MotifLiterature;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::MotifLiterature

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<motif_literature>

=cut

__PACKAGE__->table("motif_literature");

=head1 ACCESSORS

=head2 motif_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 pmid

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
  "motif_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "pmid",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
  "comment",
  { data_type => "tinytext", is_nullable => 1 },
  "order_added",
  { data_type => "tinyint", is_nullable => 1 },
);

=head1 UNIQUE CONSTRAINTS

=head2 C<Composite Unique>

=over 4

=item * L</motif_acc>

=item * L</pmid>

=item * L</order_added>

=back

=cut

__PACKAGE__->add_unique_constraint("Composite Unique", ["motif_acc", "pmid", "order_added"]);

=head1 RELATIONS

=head2 motif_acc

Type: belongs_to

Related object: L<RfamLive::Result::Motif>

=cut

__PACKAGE__->belongs_to(
  "motif_acc",
  "RfamLive::Result::Motif",
  { motif_acc => "motif_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 pmid

Type: belongs_to

Related object: L<RfamLive::Result::LiteratureReference>

=cut

__PACKAGE__->belongs_to(
  "pmid",
  "RfamLive::Result::LiteratureReference",
  { pmid => "pmid" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-07-02 08:33:40
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:oieIlixRV9x8NXem0b65pQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
