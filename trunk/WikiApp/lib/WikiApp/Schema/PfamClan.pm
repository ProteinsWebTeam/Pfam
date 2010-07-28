package WikiApp::Schema::PfamClan;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfam_clan");
__PACKAGE__->add_columns(
  "clan_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 6 },
);
__PACKAGE__->set_primary_key("clan_acc");
__PACKAGE__->has_many(
  "pfam_clan_wikis",
  "WikiApp::Schema::PfamClanWiki",
  { "foreign.clan_acc" => "self.clan_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2010-04-12 12:53:40
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:cHydgkTlmaDsFyIRT6WTQg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
