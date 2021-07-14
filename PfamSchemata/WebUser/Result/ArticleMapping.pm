use utf8;
package WebUser::Result::ArticleMapping;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

WebUser::Result::ArticleMapping

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<article_mapping>

=cut

__PACKAGE__->table("article_mapping");

=head1 ACCESSORS

=head2 accession

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 title

  data_type: 'text'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "accession",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "title",
  { data_type => "text", is_nullable => 0 },
);

=head1 PRIMARY KEY

=over 4

=item * L</accession>

=item * L</title>

=back

=cut

__PACKAGE__->set_primary_key("accession", "title");


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2021-07-14 21:37:43
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:XKHeTl5vWY6qUbLr3mAU3g
# These lines were loaded from '/nfs/production/xfam/pfam/software/Pfam/PfamSchemata/WebUser/Result/ArticleMapping.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.


package WebUser::Result::ArticleMapping;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components('Core');
__PACKAGE__->table('article_mapping');

__PACKAGE__->add_columns(
  'accession',
  { data_type => 'VARCHAR', default_value => 0, is_nullable => 0, size => 7 },
  'title',
  { data_type => 'TINYTEXT', default_value => '', is_nullable => 0, size => 255 },
);

__PACKAGE__->set_primary_key('accession','title');

__PACKAGE__->has_many(
  wikitext => 'WebUser::Result::Wikitext',
  { 'foreign.title' => 'self.title' }
);

1;

__END__

This table has the same name as one in the WikiApprove database, but this
version doesn't need the "db" column. Its use from here out doesn't require
that column.

CREATE TABLE `article_mapping` (
  `accession` varchar(7) NOT NULL,
  `title` tinytext NOT NULL,
  PRIMARY KEY  (`accession`,`title`(255))
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Pfam/PfamSchemata/WebUser/Result/ArticleMapping.pm'


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
