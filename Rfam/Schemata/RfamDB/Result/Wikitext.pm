use utf8;
package RfamDB::Result::Wikitext;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::Wikitext

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<wikitext>

=cut

__PACKAGE__->table("wikitext");

=head1 ACCESSORS

=head2 auto_wiki

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 title

  data_type: 'varchar'
  is_nullable: 0
  size: 150

=cut

__PACKAGE__->add_columns(
  "auto_wiki",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "title",
  { data_type => "varchar", is_nullable => 0, size => 150 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_wiki>

=back

=cut

__PACKAGE__->set_primary_key("auto_wiki");

=head1 UNIQUE CONSTRAINTS

=head2 C<title_UNIQUE>

=over 4

=item * L</title>

=back

=cut

__PACKAGE__->add_unique_constraint("title_UNIQUE", ["title"]);

=head1 RELATIONS

=head2 families

Type: has_many

Related object: L<RfamDB::Result::Family>

=cut

__PACKAGE__->has_many(
  "families",
  "RfamDB::Result::Family",
  { "foreign.auto_wiki" => "self.auto_wiki" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-30 11:04:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:oquHIpHBjhgryzBOspYkFg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
