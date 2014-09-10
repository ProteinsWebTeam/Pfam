package RfamLive::Result::Keyword;

use strict;
use warnings;

use base 'DBIx::Class::Core';

__PACKAGE__->table("keywords");
__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 7 },
  "rfam_id",
  { data_type => "VARCHAR", default_value => undef, is_nullable => 1, size => 40, },
  "description",
  { data_type => "VARCHAR", default_value => "NULL", is_nullable => 1, size => 100, },
  "rfam_general",
  { data_type => "LONGTEXT", default_value => undef, is_nullable => 1, size => 4294967295, },
  "literature",
  { data_type => "LONGTEXT", default_value => undef, is_nullable => 1, size => 4294967295, },
  "wiki",
  { data_type => "LONGTEXT", default_value => undef, is_nullable => 1, size => 4294967295, },
  "pdb_mappings",
  { data_type => "LONGTEXT", default_value => undef, is_nullable => 1, size => 4294967295, },
  "clan_info",
  { data_type => "LONGTEXT", default_value => undef, is_nullable => 1, size => 4294967295, },
);

__PACKAGE__->set_primary_key("rfam_acc");

__PACKAGE__->belongs_to(
  "rfam_acc",
  "RfamLive::Result::Family",
  { rfam_acc => "rfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

1;

__END__

CREATE TABLE `keywords` (
  `rfam_acc` varchar(7) NOT NULL DEFAULT '',
  `rfam_id` varchar(40) DEFAULT NULL,
  `description` varchar(100) DEFAULT 'NULL',
  `rfam_general` longtext,
  `literature` longtext,
  `wiki` longtext,
  `pdb_mappings` longtext,
  `clan_info` longtext,
  PRIMARY KEY (`rfam_acc`),
  FULLTEXT KEY `rfam_kw_idx` (`description`,`rfam_general`,`literature`,`wiki`,`pdb_mappings`,`clan_info`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8§rfam_acc
