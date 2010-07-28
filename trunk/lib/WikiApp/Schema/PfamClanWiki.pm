package WikiApp::Schema::PfamClanWiki;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfam_clan_wiki");
__PACKAGE__->add_columns(
  "auto_wiki",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "clan_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 6 },
);
__PACKAGE__->belongs_to(
  "auto_wiki",
  "WikiApp::Schema::Wikipedia",
  { auto_wiki => "auto_wiki" },
);
__PACKAGE__->belongs_to(
  "clan_acc",
  "WikiApp::Schema::PfamClan",
  { clan_acc => "clan_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2010-04-12 12:53:40
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:gPODMkOiFCeAvkJLyZPMWA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
