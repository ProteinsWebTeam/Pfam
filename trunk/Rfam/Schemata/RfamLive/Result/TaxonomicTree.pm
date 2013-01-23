use utf8;
package RfamLive::Result::TaxonomicTree;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::TaxonomicTree

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<taxonomic_tree>

=cut

__PACKAGE__->table("taxonomic_tree");

=head1 ACCESSORS

=head2 ncbi_code

  data_type: 'integer'
  is_nullable: 0

=head2 species

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 1

=head2 lft

  data_type: 'integer'
  is_nullable: 1

=head2 rgt

  data_type: 'integer'
  is_nullable: 1

=head2 parent

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 level

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=cut

__PACKAGE__->add_columns(
  "ncbi_code",
  { data_type => "integer", is_nullable => 0 },
  "species",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 1 },
  "lft",
  { data_type => "integer", is_nullable => 1 },
  "rgt",
  { data_type => "integer", is_nullable => 1 },
  "parent",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "level",
  { data_type => "varchar", is_nullable => 1, size => 100 },
);

=head1 PRIMARY KEY

=over 4

=item * L</ncbi_code>

=back

=cut

__PACKAGE__->set_primary_key("ncbi_code");


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:AAplMTd3wpwhSZU2hi0+QQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
