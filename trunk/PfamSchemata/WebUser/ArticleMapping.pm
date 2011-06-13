
package WebUser::ArticleMapping;

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
  wikitext => 'WebUser::Wikitext',
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
  PRIMARY KEY  (`accession`,`title`(256))
) ENGINE=InnoDB DEFAULT CHARSET=latin1

