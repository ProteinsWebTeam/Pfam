package RfamDB::Version;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("VERSION");
__PACKAGE__->add_columns(
  "rfam_release",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
  "rfam_release_date",
  { data_type => "DATE", default_value => "", is_nullable => 0, size => 10 },
  "number_families",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 11 },
  "embl_release",
  { data_type => "TINYTEXT", default_value => "", is_nullable => 0, size => 255 },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-02-29 10:23:32
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:hqUi9Pe4eMvno+ecdRi0iw

__PACKAGE__->set_primary_key( qw( rfam_release 
                                  embl_release ) );

# You can replace this text with custom content, and it will be preserved on regeneration
1;
