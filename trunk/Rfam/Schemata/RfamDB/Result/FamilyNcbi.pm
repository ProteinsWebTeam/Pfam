use utf8;
package RfamDB::Result::FamilyNcbi;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::FamilyNcbi

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<family_ncbi>

=cut

__PACKAGE__->table("family_ncbi");

=head1 ACCESSORS

=head2 ncbi_id

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 rfam_id

  data_type: 'varchar'
  is_nullable: 1
  size: 40

Is this really needed?

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=cut

__PACKAGE__->add_columns(
  "ncbi_id",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "rfam_id",
  { data_type => "varchar", is_nullable => 1, size => 40 },
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
);

=head1 RELATIONS

=head2 ncbi

Type: belongs_to

Related object: L<RfamDB::Result::Taxonomy>

=cut

__PACKAGE__->belongs_to(
  "ncbi",
  "RfamDB::Result::Taxonomy",
  { ncbi_id => "ncbi_id" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 rfam_acc

Type: belongs_to

Related object: L<RfamDB::Result::Family>

=cut

__PACKAGE__->belongs_to(
  "rfam_acc",
  "RfamDB::Result::Family",
  { rfam_acc => "rfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:EAAZfPQ7u9ZXvVvGOdRwNg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
