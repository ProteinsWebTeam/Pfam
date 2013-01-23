use utf8;
package RfamLive::Result::LiteratureReference;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::LiteratureReference

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
  is_auto_increment: 1
  is_nullable: 0

=head2 pmid

  data_type: 'integer'
  is_nullable: 1

Ever have reference without a PMID?

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
  "auto_lit",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "pmid",
  { data_type => "integer", is_nullable => 1 },
  "title",
  { data_type => "tinytext", is_nullable => 1 },
  "author",
  { data_type => "text", is_nullable => 1 },
  "journal",
  { data_type => "tinytext", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_lit>

=back

=cut

__PACKAGE__->set_primary_key("auto_lit");

=head1 RELATIONS

=head2 clan_literature_references

Type: has_many

Related object: L<RfamLive::Result::ClanLiteratureReference>

=cut

__PACKAGE__->has_many(
  "clan_literature_references",
  "RfamLive::Result::ClanLiteratureReference",
  { "foreign.auto_lit" => "self.auto_lit" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 family_literature_references

Type: has_many

Related object: L<RfamLive::Result::FamilyLiteratureReference>

=cut

__PACKAGE__->has_many(
  "family_literature_references",
  "RfamLive::Result::FamilyLiteratureReference",
  { "foreign.literature_reference_auto_lit" => "self.auto_lit" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:9F4sih6rPS4HyG77agi4Rg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
