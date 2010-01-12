package PfamJobs::JobStream;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("job_stream");
__PACKAGE__->add_columns(
  "id",
  { data_type => "BIGINT", default_value => 0, is_nullable => 0, size => 20 },
  "stdin",
  {
    data_type => "LONGTEXT",
    default_value => "",
    is_nullable => 0,
    size => 4294967295,
  },
  "stdout",
  {
    data_type => "LONGTEXT",
    default_value => "",
    is_nullable => 0,
    size => 4294967295,
  },
  "stderr",
  {
    data_type => "LONGTEXT",
    default_value => "",
    is_nullable => 0,
    size => 4294967295,
  },
);
__PACKAGE__->set_primary_key("id");
__PACKAGE__->belongs_to("id", "PfamJobs::JobHistory", { id => "id" });


# Created by DBIx::Class::Schema::Loader v0.04003 @ 2009-07-22 11:06:31
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:13o0RtQlZXS9N46pGYbv9Q

# You can replace this text with custom content, and it will be preserved on regeneration
1;
