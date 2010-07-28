package WikiApp::Schema::PfamaWiki;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA_wiki");
__PACKAGE__->add_columns(
  "auto_wiki",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "pfama_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
);
__PACKAGE__->belongs_to(
  "auto_wiki",
  "WikiApp::Schema::Wikipedia",
  { auto_wiki => "auto_wiki" },
);
__PACKAGE__->belongs_to(
  "pfama_acc",
  "WikiApp::Schema::Pfama",
  { pfama_acc => "pfama_acc" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2010-04-12 12:53:40
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ZEcNujBcs/YyDxMmv9fTXQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
