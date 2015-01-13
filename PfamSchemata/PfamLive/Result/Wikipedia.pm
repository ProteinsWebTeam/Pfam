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

=head2 clan_wikis

Type: has_many

Related object: L<PfamLive::Result::ClanWiki>

=cut

__PACKAGE__->has_many(
  "clan_wikis",
  "PfamLive::Result::ClanWiki",
  { "foreign.auto_wiki" => "self.auto_wiki" },
  { cascade_copy => 0, cascade_delete => 0 },
);

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


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:XU9y8koSI9b51cS2WPjfhw
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/Wikipedia.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

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

=head2 clan_wikis

Type: has_many

Related object: L<PfamLive::Result::ClanWiki>

=cut

__PACKAGE__->has_many(
  "clan_wikis",
  "PfamLive::Result::ClanWiki",
  { "foreign.auto_wiki" => "self.auto_wiki" },
  { cascade_copy => 0, cascade_delete => 0 },
);

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


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:jLDUj/0Gqx69oOcn1cBZtg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/Wikipedia.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
