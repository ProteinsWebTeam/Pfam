package PfamLive::Wikipedia;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("wikipedia");
__PACKAGE__->add_columns(
  "auto_wiki",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "title",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "wikitext",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
);
__PACKAGE__->set_primary_key("auto_wiki");
__PACKAGE__->add_unique_constraint("UQ_wikipedia_1", ["title"]);
__PACKAGE__->has_many(
  "clan_wikis",
  "PfamLive::ClanWiki",
  { "foreign.auto_wiki" => "self.auto_wiki" },
);
__PACKAGE__->has_many(
  "pfama_wikis",
  "PfamLive::PfamaWiki",
  { "foreign.auto_wiki" => "self.auto_wiki" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2010-03-23 10:27:02
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ZwxUU71nu+CBkf344DLdeQ


__PACKAGE__->many_to_many(
  'pfams' => 'pfama_wikis', 'auto_pfama'
);
 
1;
