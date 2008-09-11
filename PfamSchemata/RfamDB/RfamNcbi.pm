package RfamDB::RfamNcbi;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("rfam_ncbi");
__PACKAGE__->add_columns(
  "auto_rfam",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "rfam_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
  "rfam_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 40 },
  "ncbi_code",
  { data_type => "INT", default_value => 0, is_nullable => 1, size => 10 },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2008-09-11 09:48:06
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:mNhH6bqLckeNe+4QPrWQ9g


# You can replace this text with custom content, and it will be preserved on regeneration
1;
