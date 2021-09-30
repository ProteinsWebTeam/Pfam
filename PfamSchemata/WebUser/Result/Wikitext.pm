use utf8;
package WebUser::Result::Wikitext;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

WebUser::Result::Wikitext

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<wikitext>

=cut

__PACKAGE__->table("wikitext");

=head1 ACCESSORS

=head2 title

  data_type: 'text'
  is_nullable: 0

=head2 text

  data_type: 'longtext'
  is_nullable: 1

=head2 approved_revision

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "title",
  { data_type => "text", is_nullable => 0 },
  "text",
  { data_type => "longtext", is_nullable => 1 },
  "approved_revision",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 1,
  },
);

=head1 PRIMARY KEY

=over 4

=item * L</title>

=back

=cut

__PACKAGE__->set_primary_key("title");


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2021-07-14 21:37:43
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Rm3aUGngMqvLMZBX+SK94w
# These lines were loaded from '/nfs/production/xfam/pfam/software/Pfam/PfamSchemata/WebUser/Result/Wikitext.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

package WebUser::Result::Wikitext;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("wikitext");
__PACKAGE__->add_columns(
  "title",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "text",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "approved_revision",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
);
__PACKAGE__->set_primary_key("title");

1;

__END__
CREATE TABLE `wikitext` (
  `title` tinytext NOT NULL,
  `text` longtext character set utf8,
  `approved_revision` int(10) unsigned default '0',
  PRIMARY KEY  (`title`(255))
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Pfam/PfamSchemata/WebUser/Result/Wikitext.pm'


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
