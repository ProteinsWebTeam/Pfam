package PfamJobs::JobHistory;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("job_history");
__PACKAGE__->add_columns(
  "id",
  { data_type => "BIGINT", default_value => undef, is_nullable => 0, size => 20 },
  "job_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 40 },
  "status",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 5 },
  "lsf_id",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 19 },
  "entity_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 20 },
  "entity_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
  "entity_size",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "job_type",
  { data_type => "ENUM", default_value => undef, is_nullable => 1, size => 6 },
  "options",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "opened",
  {
    data_type => "DATETIME",
    default_value => "0000-00-00 00:00:00",
    is_nullable => 0,
    size => 19,
  },
  "closed",
  {
    data_type => "DATETIME",
    default_value => "0000-00-00 00:00:00",
    is_nullable => 0,
    size => 19,
  },
  "started",
  {
    data_type => "DATETIME",
    default_value => "0000-00-00 00:00:00",
    is_nullable => 0,
    size => 19,
  },
  "user_id",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
);
__PACKAGE__->set_primary_key("id");
__PACKAGE__->has_many(
  "job_streams",
  "PfamJobs::JobStream",
  { "foreign.id" => "self.id" },
);


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-07-22 11:06:31
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:N7z1gLorhA6OQScdkqokAA

# You can replace this text with custom content, and it will be preserved on regeneration
1;
