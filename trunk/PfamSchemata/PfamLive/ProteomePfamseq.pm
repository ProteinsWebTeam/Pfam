package PfamLive::ProteomePfamseq;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("proteome_pfamseq");
__PACKAGE__->add_columns(
  "auto_proteome",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "auto_pfamseq",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 10 },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-08-20 12:44:49
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3hIzTRLcYunl5wsnS6wNqQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
