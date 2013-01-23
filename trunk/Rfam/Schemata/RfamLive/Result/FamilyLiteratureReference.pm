use utf8;
package RfamLive::Result::FamilyLiteratureReference;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::FamilyLiteratureReference

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<family_literature_reference>

=cut

__PACKAGE__->table("family_literature_reference");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 auto_lit

  data_type: 'integer'
  default_value: 0
  is_nullable: 0

=head2 comment

  data_type: 'tinytext'
  is_nullable: 1

=head2 order_added

  data_type: 'tinyint'
  is_nullable: 1

=head2 literature_reference_auto_lit

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "auto_lit",
  { data_type => "integer", default_value => 0, is_nullable => 0 },
  "comment",
  { data_type => "tinytext", is_nullable => 1 },
  "order_added",
  { data_type => "tinyint", is_nullable => 1 },
  "literature_reference_auto_lit",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
);

=head1 RELATIONS

=head2 literature_reference_auto_lit

Type: belongs_to

Related object: L<RfamLive::Result::LiteratureReference>

=cut

__PACKAGE__->belongs_to(
  "literature_reference_auto_lit",
  "RfamLive::Result::LiteratureReference",
  { auto_lit => "literature_reference_auto_lit" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 rfam_acc

Type: belongs_to

Related object: L<RfamLive::Result::Family>

=cut

__PACKAGE__->belongs_to(
  "rfam_acc",
  "RfamLive::Result::Family",
  { rfam_acc => "rfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:b1EptHKZ1JHUaH6u2/ODPA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
