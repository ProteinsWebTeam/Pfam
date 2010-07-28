package WikiApp::Schema::CronLog;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("cron_log");
__PACKAGE__->add_columns(
  "patchcron",
  {
    data_type => "DATETIME",
    default_value => undef,
    is_nullable => 1,
    size => 19,
  },
  "wikicron",
  {
    data_type => "DATETIME",
    default_value => undef,
    is_nullable => 1,
    size => 19,
  },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2010-04-12 12:53:40
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:wI5MDjZbq94IAxU9bLq8cQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
