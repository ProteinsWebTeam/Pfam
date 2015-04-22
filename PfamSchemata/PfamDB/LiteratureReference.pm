use utf8;
package PfamDB::LiteratureReference;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::LiteratureReference

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<literature_reference>

=cut

__PACKAGE__->table("literature_reference");

=head1 ACCESSORS

=head2 auto_lit

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 pmid

  data_type: 'integer'
  is_nullable: 1

=head2 title

  data_type: 'mediumtext'
  is_nullable: 1

=head2 author

  data_type: 'mediumtext'
  is_nullable: 1

=head2 journal

  data_type: 'tinytext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_lit",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "pmid",
  { data_type => "integer", is_nullable => 1 },
  "title",
  { data_type => "mediumtext", is_nullable => 1 },
  "author",
  { data_type => "mediumtext", is_nullable => 1 },
  "journal",
  { data_type => "tinytext", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_lit>

=back

=cut

__PACKAGE__->set_primary_key("auto_lit");

=head1 UNIQUE CONSTRAINTS

=head2 C<IX_literature_references_1>

=over 4

=item * L</pmid>

=back

=cut

__PACKAGE__->add_unique_constraint("IX_literature_references_1", ["pmid"]);

=head1 RELATIONS

=head2 clan_lit_refs

Type: has_many

Related object: L<PfamDB::ClanLitRef>

=cut

__PACKAGE__->has_many(
  "clan_lit_refs",
  "PfamDB::ClanLitRef",
  { "foreign.auto_lit" => "self.auto_lit" },
  undef,
);

=head2 pfama_literature_references

Type: has_many

Related object: L<PfamDB::PfamaLiteratureReference>

=cut

__PACKAGE__->has_many(
  "pfama_literature_references",
  "PfamDB::PfamaLiteratureReference",
  { "foreign.auto_lit" => "self.auto_lit" },
  undef,
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:tMM9+NUjMNORYks16tiujQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
