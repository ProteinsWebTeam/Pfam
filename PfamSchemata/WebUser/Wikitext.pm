package WebUser::Wikitext;

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
  PRIMARY KEY  (`title`(256))
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

