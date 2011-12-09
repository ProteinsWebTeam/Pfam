package PfamLive::Interpro;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("interpro");
__PACKAGE__->add_columns(
  "auto_pfama",
  { data_type => "INT", default_value => 0, is_nullable => 0, size => 5 },
  "interpro_id",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 0,
    size => 255,
  },
  "abstract",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 0,
    size => 4294967295,
  },
);
__PACKAGE__->add_unique_constraint("UQ_interpro_1", ["auto_pfama"]);
__PACKAGE__->belongs_to(
  "auto_pfama",
  "PfamLive::Pfama",
  { auto_pfama => "auto_pfama" },
);


# Created by DBIx::Class::Schema::Loader v0.04005 @ 2009-01-17 10:09:48
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:lI3rC1SwrkXzLdm5LNyuDA

__PACKAGE__->add_unique_constraint( 
  auto_ip_unq => [ qw( auto_pfama interpro_id ) ] 
); 

# You can replace this text with custom content, and it will be preserved on regeneration
1;
