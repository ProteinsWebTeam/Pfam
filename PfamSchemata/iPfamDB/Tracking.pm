package iPfamDB::Tracking;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("tracking");
__PACKAGE__->add_columns(
  "pdb_id",
  {
    data_type => "VARCHAR",
    default_value => "NULL",
    is_nullable => 0,
    size => 5,
  },
  "started",
  {
    data_type => "DATETIME",
    default_value => "0000-00-00 00:00:00",
    is_nullable => 0,
    size => 19,
  },
  "finished",
  {
    data_type => "DATETIME",
    default_value => "0000-00-00 00:00:00",
    is_nullable => 0,
    size => 19,
  },
  "job_type",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 50 },
);
__PACKAGE__->set_primary_key("pdb_id");


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2008-02-26 14:01:41
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:nEMrhhEJP07fnKGBjFI5Bw


# You can replace this text with custom content, and it will be preserved on regeneration
1;
