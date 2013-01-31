use utf8;
package RfamLive::Result::TaxonomyWebsearch;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::TaxonomyWebsearch

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<taxonomy_websearch>

=cut

__PACKAGE__->table("taxonomy_websearch");

=head1 ACCESSORS

=head2 ncbi_id

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 1

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

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 level

  data_type: 'varchar'
  is_nullable: 1
  size: 200

=head2 minimal

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 0

=head2 rank

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=cut

__PACKAGE__->add_columns(
  "ncbi_id",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 1,
  },
  "species",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 1 },
  "lft",
  { data_type => "integer", is_nullable => 1 },
  "rgt",
  { data_type => "integer", is_nullable => 1 },
  "parent",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "level",
  { data_type => "varchar", is_nullable => 1, size => 200 },
  "minimal",
  { data_type => "tinyint", default_value => 0, is_nullable => 0 },
  "rank",
  { data_type => "varchar", is_nullable => 1, size => 100 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-31 15:25:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:i0HvP23WNgDFf2AD1PdvGQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
