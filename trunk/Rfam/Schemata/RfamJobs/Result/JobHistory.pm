use utf8;
package RfamJobs::Result::JobHistory;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamJobs::Result::JobHistory

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 COMPONENTS LOADED

=over 4

=item * L<DBIx::Class::InflateColumn::DateTime>

=back

=cut

__PACKAGE__->load_components("InflateColumn::DateTime");

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
  size: 20

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
  { data_type => "varchar", is_nullable => 0, size => 20 },
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

=head1 RELATIONS

=head2 job_streams

Type: has_many

Related object: L<RfamJobs::Result::JobStream>

=cut

__PACKAGE__->has_many(
  "job_streams",
  "RfamJobs::Result::JobStream",
  { "foreign.id" => "self.id" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2014-07-10 13:20:42
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:dL7uQfOGoMo1EpL7Nx3L1Q


sub run {
  my $self = shift;
   $self->update( {
     status  => 'RUN',
     started => \'NOW()'
   } );
}

sub done {
  my $self = shift;
  $self->update( {
    status  => 'DONE',
    closed => \'NOW()'
  } );
}

sub fail {
  my $self = shift;
  $self->update({
    status  => 'FAIL',
    closed => \'NOW()'
  } );
}

1;
