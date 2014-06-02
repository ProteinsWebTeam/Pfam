use utf8;
package RfamLive::Result::Taxonomy;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::Taxonomy

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<taxonomy>

=cut

__PACKAGE__->table("taxonomy");

=head1 ACCESSORS

=head2 ncbi_id

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 species

  data_type: 'varchar'
  default_value: (empty string)
  is_nullable: 0
  size: 100

=head2 tax_string

  data_type: 'mediumtext'
  is_nullable: 1

=head2 tree_display_name

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 align_display_name

  data_type: 'varchar'
  is_nullable: 1
  size: 50

=cut

__PACKAGE__->add_columns(
  "ncbi_id",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "species",
  { data_type => "varchar", default_value => "", is_nullable => 0, size => 100 },
  "tax_string",
  { data_type => "mediumtext", is_nullable => 1 },
  "tree_display_name",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "align_display_name",
  { data_type => "varchar", is_nullable => 1, size => 50 },
);

=head1 PRIMARY KEY

=over 4

=item * L</ncbi_id>

=back

=cut

__PACKAGE__->set_primary_key("ncbi_id");

=head1 RELATIONS

=head2 family_ncbis

Type: has_many

Related object: L<RfamLive::Result::FamilyNcbi>

=cut

__PACKAGE__->has_many(
  "family_ncbis",
  "RfamLive::Result::FamilyNcbi",
  { "foreign.ncbi_id" => "self.ncbi_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 rfamseqs

Type: has_many

Related object: L<RfamLive::Result::Rfamseq>

=cut

__PACKAGE__->has_many(
  "rfamseqs",
  "RfamLive::Result::Rfamseq",
  { "foreign.ncbi_id" => "self.ncbi_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-06-02 12:58:16
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:lZKy2uppmjVo7Nf/Meqx5A


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
