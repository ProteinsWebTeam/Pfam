use utf8;
package PfamJobs::Result::JobStream;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamJobs::Result::JobStream

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<job_stream>

=cut

__PACKAGE__->table("job_stream");

=head1 ACCESSORS

=head2 id

  data_type: 'bigint'
  default_value: 0
  is_nullable: 0

=head2 stdin

  data_type: 'longtext'
  is_nullable: 0

=head2 stdout

  data_type: 'longtext'
  is_nullable: 0

=head2 stderr

  data_type: 'longtext'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "id",
  { data_type => "bigint", default_value => 0, is_nullable => 0 },
  "stdin",
  { data_type => "longtext", is_nullable => 0 },
  "stdout",
  { data_type => "longtext", is_nullable => 0 },
  "stderr",
  { data_type => "longtext", is_nullable => 0 },
);

=head1 PRIMARY KEY

=over 4

=item * L</id>

=back

=cut

__PACKAGE__->set_primary_key("id");


# Created by DBIx::Class::Schema::Loader v0.07049 @ 2022-10-18 11:46:39
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:QmdlrovxuQ8nvNh5t6Jd/w
# These lines were loaded from '/hps/software/users/agb/pfam/software/Pfam/PfamSchemata/PfamJobs/Result/JobStream.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamJobs::Result::JobStream;

use strict;
use warnings;

use base 'DBIx::Class::Core';

#__PACKAGE__->load_components("Core");
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
# End of lines loaded from '/hps/software/users/agb/pfam/software/Pfam/PfamSchemata/PfamJobs/Result/JobStream.pm'
# These lines were loaded from '/hps/software/users/agb/pfam/software/Pfam/PfamSchemata/PfamJobs/Result/JobStream.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamJobs::Result::JobStream;

use strict;
use warnings;

use base 'DBIx::Class::Core';

#__PACKAGE__->load_components("Core");
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
# End of lines loaded from '/hps/software/users/agb/pfam/software/Pfam/PfamSchemata/PfamJobs/Result/JobStream.pm'

# You can replace this text with custom content, and it will be preserved on regeneration
1;
