
package WikiApprove::ArticleMapping;

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
  'db',
  { data_type => 'ENUM', default_value => '', is_nullable => 0, size => 4 },
);

__PACKAGE__->set_primary_key('accession','title');

1;

__END__
CREATE TABLE `article_mapping` (
  `accession` varchar(7) NOT NULL,
  `title` tinytext NOT NULL,
  `db` enum('pfam','rfam') NOT NULL,
  PRIMARY KEY  (`accession`,`title`(256))
) ENGINE=InnoDB DEFAULT CHARSET=latin1

