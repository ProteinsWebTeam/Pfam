package DfamLive::Schema::Result::Wikipedia;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamLive::Schema::Result::Wikipedia

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

Related object: L<DfamLive::Schema::Result::DfamWiki>

=cut

__PACKAGE__->has_many(
  "dfam_wikis",
  "DfamLive::Schema::Result::DfamWiki",
  { "foreign.auto_wiki" => "self.auto_wiki" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-03-13 22:18:33
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:68LVWfvf14+xRHuH5Bc5rQ
# These lines were loaded from '/opt/dfam/code/Schemata/DfamLive/Schema/Result/Wikipedia.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

package DfamLive::Schema::Result::Wikipedia;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamLive::Schema::Result::Wikipedia

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

Related object: L<DfamLive::Schema::Result::DfamWiki>

=cut

__PACKAGE__->has_many(
  "dfam_wikis",
  "DfamLive::Schema::Result::DfamWiki",
  { "foreign.auto_wiki" => "self.auto_wiki" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-03-13 22:12:32
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:tyz4W6MlwR0O1Ktqub13IA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
# End of lines loaded from '/opt/dfam/code/Schemata/DfamLive/Schema/Result/Wikipedia.pm' 


# You can replace this text with custom content, and it will be preserved on regeneration
1;
