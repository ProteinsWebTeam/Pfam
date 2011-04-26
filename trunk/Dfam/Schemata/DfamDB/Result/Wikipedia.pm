package DfamDB::Result::Wikipedia;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamDB::Result::Wikipedia

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
__PACKAGE__->set_primary_key("auto_wiki");

=head1 RELATIONS

=head2 dfam_wikis

Type: has_many

Related object: L<DfamDB::Result::DfamWiki>

=cut

__PACKAGE__->has_many(
  "dfam_wikis",
  "DfamDB::Result::DfamWiki",
  { "foreign.auto_wiki" => "self.auto_wiki" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-01-11 15:01:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:GKGwM1vyfMLftHfVJSGH4w


# You can replace this text with custom content, and it will be preserved on regeneration
1;
