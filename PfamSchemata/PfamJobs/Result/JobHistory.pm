use utf8;
package PfamJobs::Result::JobHistory;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamJobs::Result::JobHistory

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<job_history>

=cut

__PACKAGE__->table("job_history");

=head1 ACCESSORS

=head2 id

  data_type: 'bigint'
  is_auto_increment: 1
  is_nullable: 0

=head2 job_id

  data_type: 'varchar'
  is_nullable: 0
  size: 40

=head2 status

  data_type: 'varchar'
  is_nullable: 0
  size: 5

=head2 lsf_id

  data_type: 'bigint'
  is_nullable: 1

=head2 entity_id

  data_type: 'varchar'
  is_nullable: 0
  size: 30

=head2 entity_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 9

=head2 entity_size

  data_type: 'mediumint'
  is_nullable: 1

=head2 job_type

  data_type: 'enum'
  extra: {list => ["family","clan","ancillary"]}
  is_nullable: 0

=head2 options

  data_type: 'varchar'
  is_nullable: 1
  size: 255

=head2 opened

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  default_value: '0000-00-00 00:00:00'
  is_nullable: 0

=head2 closed

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  default_value: '0000-00-00 00:00:00'
  is_nullable: 0

=head2 started

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  default_value: '0000-00-00 00:00:00'
  is_nullable: 0

=head2 user_id

  data_type: 'varchar'
  is_nullable: 1
  size: 255

=cut

__PACKAGE__->add_columns(
  "id",
  { data_type => "bigint", is_auto_increment => 1, is_nullable => 0 },
  "job_id",
  { data_type => "varchar", is_nullable => 0, size => 40 },
  "status",
  { data_type => "varchar", is_nullable => 0, size => 5 },
  "lsf_id",
  { data_type => "bigint", is_nullable => 1 },
  "entity_id",
  { data_type => "varchar", is_nullable => 0, size => 30 },
  "entity_acc",
  { data_type => "varchar", is_nullable => 0, size => 9 },
  "entity_size",
  { data_type => "mediumint", is_nullable => 1 },
  "job_type",
  {
    data_type => "enum",
    extra => { list => ["family", "clan", "ancillary"] },
    is_nullable => 0,
  },
  "options",
  { data_type => "varchar", is_nullable => 1, size => 255 },
  "opened",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    default_value => "0000-00-00 00:00:00",
    is_nullable => 0,
  },
  "closed",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    default_value => "0000-00-00 00:00:00",
    is_nullable => 0,
  },
  "started",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    default_value => "0000-00-00 00:00:00",
    is_nullable => 0,
  },
  "user_id",
  { data_type => "varchar", is_nullable => 1, size => 255 },
);

=head1 PRIMARY KEY

=over 4

=item * L</id>

=back

=cut

__PACKAGE__->set_primary_key("id");


# Created by DBIx::Class::Schema::Loader v0.07049 @ 2022-10-18 11:46:39
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:GyeL2dXVCzqd8jyGo9FURw
# These lines were loaded from '/hps/software/users/agb/pfam/software/Pfam/PfamSchemata/PfamJobs/Result/JobHistory.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamJobs::Result::JobHistory;

use strict;
use warnings;

use base 'DBIx::Class::Core';

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

sub run {
  my $self = shift;
   $self->update(
      {
        status  => 'RUN',
        started => \'NOW()'
      });
}

sub done {
  my $self = shift;
  $self->update(
     {
       status  => 'DONE',
       closed => \'NOW()'
      }) 
  
}

sub fail {
  my $self = shift;
  $self->update({
        status  => 'FAIL',
        closed => \'NOW()'
      });
}

1;
# End of lines loaded from '/hps/software/users/agb/pfam/software/Pfam/PfamSchemata/PfamJobs/Result/JobHistory.pm'
# These lines were loaded from '/hps/software/users/agb/pfam/software/Pfam/PfamSchemata/PfamJobs/Result/JobHistory.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamJobs::Result::JobHistory;

use strict;
use warnings;

use base 'DBIx::Class::Core';

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

sub run {
  my $self = shift;
   $self->update(
      {
        status  => 'RUN',
        started => \'NOW()'
      });
}

sub done {
  my $self = shift;
  $self->update(
     {
       status  => 'DONE',
       closed => \'NOW()'
      }) 
  
}

sub fail {
  my $self = shift;
  $self->update({
        status  => 'FAIL',
        closed => \'NOW()'
      });
}

1;
# End of lines loaded from '/hps/software/users/agb/pfam/software/Pfam/PfamSchemata/PfamJobs/Result/JobHistory.pm'

# You can replace this text with custom content, and it will be preserved on regeneration

sub run {
  my $self = shift;
   $self->update(
      {
        status  => 'RUN',
        started => \'NOW()'
      });
}

sub done {
  my $self = shift;
  $self->update(
     {
       status  => 'DONE',
       closed => \'NOW()'
      }) 
  
}

sub fail {
  my $self = shift;
  $self->update({
        status  => 'FAIL',
        closed => \'NOW()'
      });
}

1;
