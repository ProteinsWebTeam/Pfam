use utf8;
package RfamJobs::Result::JobStream;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamJobs::Result::JobStream

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

=head1 TABLE: C<job_stream>

=cut

__PACKAGE__->table("job_stream");

=head1 ACCESSORS

=head2 id

  data_type: 'bigint'
  default_value: 0
  is_foreign_key: 1
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
  {
    data_type      => "bigint",
    default_value  => 0,
    is_foreign_key => 1,
    is_nullable    => 0,
  },
  "stdin",
  { data_type => "longtext", is_nullable => 0 },
  "stdout",
  { data_type => "longtext", is_nullable => 0 },
  "stderr",
  { data_type => "longtext", is_nullable => 0 },
);

=head1 RELATIONS

=head2 id

Type: belongs_to

Related object: L<RfamJobs::Result::JobHistory>

=cut

__PACKAGE__->belongs_to(
  "id",
  "RfamJobs::Result::JobHistory",
  { id => "id" },
  { is_deferrable => 1, on_delete => "RESTRICT", on_update => "RESTRICT" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2014-07-10 13:26:11
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:5VgbIakE+M7DOm2/hgoM3Q


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
