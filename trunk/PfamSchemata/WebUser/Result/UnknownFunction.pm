
package WebUser::Result::UnknownFunction;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components('Core');
__PACKAGE__->table('unknown_function');

__PACKAGE__->add_columns(
  'accession',
  { data_type => 'VARCHAR', default_value => 0, is_nullable => 0, size => 7 },
  'db',
  { data_type => 'ENUM', default_value => '', is_nullable => 0, size => 4 },
);

__PACKAGE__->set_primary_key('accession');

1;

__END__
CREATE TABLE `unknown_function` (
  `accession` varchar(7) NOT NULL,
  `db` enum('pfam','rfam') default NULL,
  PRIMARY KEY  (`accession`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8

