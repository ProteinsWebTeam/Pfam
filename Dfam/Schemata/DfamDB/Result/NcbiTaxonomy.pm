package DfamDB::Result::NcbiTaxonomy;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamDB::Result::NcbiTaxonomy

=cut

__PACKAGE__->table("ncbi_taxonomy");

=head1 ACCESSORS

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 species

  data_type: 'varchar'
  is_nullable: 0
  size: 100

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "species",
  { data_type => "varchar", is_nullable => 0, size => 100 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 0 },
);
__PACKAGE__->set_primary_key("ncbi_taxid");

=head1 RELATIONS

=head2 dfamseqs

Type: has_many

Related object: L<DfamDB::Result::Dfamseq>

=cut

__PACKAGE__->has_many(
  "dfamseqs",
  "DfamDB::Result::Dfamseq",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 taxonomies

Type: has_many

Related object: L<DfamDB::Result::Taxonomy>

=cut

__PACKAGE__->has_many(
  "taxonomies",
  "DfamDB::Result::Taxonomy",
  { "foreign.ncbi_taxid" => "self.ncbi_taxid" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-01-11 15:01:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:lbUu/Y8tZUm1NkIZ40se3Q


# You can replace this text with custom content, and it will be preserved on regeneration
1;
