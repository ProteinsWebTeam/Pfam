package PfamLive::PfamaWiki;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("pfamA_wiki");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 5 },
  "auto_wiki",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
);
__PACKAGE__->belongs_to(
  "auto_wiki",
  "PfamLive::Wikipedia",
  { auto_wiki => "auto_wiki" },
);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2010-03-23 10:27:02
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:iIwYdTBVEb1ChNkCPXGTqg


__PACKAGE__->set_primary_key( 'auto_pfama', 'auto_wiki' );

1;
