use utf8;
package PfamLive::Result::Wikipedia;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Wikipedia

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<wikipedia>

=cut

__PACKAGE__->table("wikipedia");

=head1 ACCESSORS

=head2 auto_wiki

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 title

  data_type: 'tinytext'
  is_nullable: 0

=head2 wikitext

  data_type: 'longtext'
  is_nullable: 1

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
  { data_type => "tinytext", is_nullable => 0 },
  "wikitext",
  { data_type => "longtext", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_wiki>

=back

=cut

__PACKAGE__->set_primary_key("auto_wiki");

=head1 RELATIONS

=head2 pfam_a_wikis

Type: has_many

Related object: L<PfamLive::Result::PfamAWiki>

=cut

__PACKAGE__->has_many(
  "pfam_a_wikis",
  "PfamLive::Result::PfamAWiki",
  { "foreign.auto_wiki" => "self.auto_wiki" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2016-05-17 16:00:14
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:fBBHGv/uJADeAYiu5tQkeA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
