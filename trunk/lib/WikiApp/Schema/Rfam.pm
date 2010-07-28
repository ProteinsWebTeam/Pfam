package WikiApp::Schema::Rfam;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("rfam");
__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
);
__PACKAGE__->set_primary_key("rfam_acc");
__PACKAGE__->has_many(
  "rfam_wikis",
  "WikiApp::Schema::RfamWiki",
  { "foreign.rfam_acc" => "self.rfam_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2010-04-12 12:53:40
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:AL/UtA8ZEKLC3tsJn0GIEQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
