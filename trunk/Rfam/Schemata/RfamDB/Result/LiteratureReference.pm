use utf8;
package RfamDB::Result::LiteratureReference;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::LiteratureReference

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<literature_reference>

=cut

__PACKAGE__->table("literature_reference");

=head1 ACCESSORS

=head2 pmid

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 title

  data_type: 'tinytext'
  is_nullable: 1

=head2 author

  data_type: 'text'
  is_nullable: 1

=head2 journal

  data_type: 'tinytext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pmid",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "title",
  { data_type => "tinytext", is_nullable => 1 },
  "author",
  { data_type => "text", is_nullable => 1 },
  "journal",
  { data_type => "tinytext", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pmid>

=back

=cut

__PACKAGE__->set_primary_key("pmid");

=head1 RELATIONS

=head2 motif_literature_references

Type: has_many

Related object: L<RfamDB::Result::MotifLiterature>

=cut

__PACKAGE__->has_many(
  "motif_literature_references",
  "RfamDB::Result::MotifLiterature",
  { "foreign.pmid" => "self.pmid" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 clan_literature_references

Type: has_many

Related object: L<RfamDB::Result::ClanLiteratureReference>

=cut

__PACKAGE__->has_many(
  "clan_literature_references",
  "RfamDB::Result::ClanLiteratureReference",
  { "foreign.pmid" => "self.pmid" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 family_literature_references

Type: has_many

Related object: L<RfamDB::Result::FamilyLiteratureReference>

=cut

__PACKAGE__->has_many(
  "family_literature_references",
  "RfamDB::Result::FamilyLiteratureReference",
  { "foreign.pmid" => "self.pmid" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:mOIeqhae2bNKb+iIrxt5FA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
