package WikiApp::Schema::Pfama;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA");
__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
);
__PACKAGE__->set_primary_key("pfama_acc");
__PACKAGE__->has_many(
  "pfama_wikis",
  "WikiApp::Schema::PfamaWiki",
  { "foreign.pfama_acc" => "self.pfama_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2010-04-12 12:53:40
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:hIQo6mGE3ch0UPuUjc9ajQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
