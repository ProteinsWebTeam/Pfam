use utf8;
package PfamLive::Result::Author;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Author

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<author>

=cut

__PACKAGE__->table("author");

=head1 ACCESSORS

=head2 author_id

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 author

  data_type: 'tinytext'
  is_nullable: 0

=head2 orcid

  data_type: 'varchar'
  is_nullable: 1
  size: 19

=cut

__PACKAGE__->add_columns(
  "author_id",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "author",
  { data_type => "tinytext", is_nullable => 0 },
  "orcid",
  { data_type => "varchar", is_nullable => 1, size => 19 },
);

=head1 PRIMARY KEY

=over 4

=item * L</author_id>

=back

=cut

__PACKAGE__->set_primary_key("author_id");

=head1 UNIQUE CONSTRAINTS

=head2 C<au_id_orcid>

=over 4

=item * L</author_id>

=item * L</orcid>

=back

=cut

__PACKAGE__->add_unique_constraint("au_id_orcid", ["author_id", "orcid"]);
__PACKAGE__->add_unique_constraint("orcid", ["orcid"]);

=head1 RELATIONS

=head2 pfam_a_authors

Type: has_many

Related object: L<PfamLive::Result::PfamAAuthor>

=cut

__PACKAGE__->has_many(
  "pfam_a_authors",
  "PfamLive::Result::PfamAAuthor",
  { "foreign.author_id" => "self.author_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2017-11-30 14:55:37
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:iIM2kjYpGbTVa0zx9y07NA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
