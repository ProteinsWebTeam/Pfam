package DfamDB::Result::Taxonomy;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamDB::Result::Taxonomy

=cut

__PACKAGE__->table("taxonomy");

=head1 ACCESSORS

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
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

  data_type: 'varchar'
  is_nullable: 1
  size: 100

=head2 level

  data_type: 'varchar'
  is_nullable: 1
  size: 100

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
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
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
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "level",
  { data_type => "varchar", is_nullable => 1, size => 100 },
  "minimal",
  { data_type => "tinyint", default_value => 0, is_nullable => 0 },
  "rank",
  { data_type => "varchar", is_nullable => 1, size => 100 },
);

=head1 RELATIONS

=head2 ncbi_taxid

Type: belongs_to

Related object: L<DfamDB::Result::NcbiTaxonomy>

=cut

__PACKAGE__->belongs_to(
  "ncbi_taxid",
  "DfamDB::Result::NcbiTaxonomy",
  { ncbi_taxid => "ncbi_taxid" },
  {
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "CASCADE",
  },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-01-11 15:01:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:2EnM+ZWThczTl0IE2a7XpA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
