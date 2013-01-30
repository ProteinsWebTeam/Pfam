use utf8;
package RfamLive::Result::GenomeData;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::GenomeData

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<_genome_data>

=cut

__PACKAGE__->table("_genome_data");

=head1 ACCESSORS

=head2 data_file

  data_type: 'mediumtext'
  is_nullable: 0

=head2 author

  data_type: 'varchar'
  is_nullable: 0
  size: 45

=head2 uuid

  data_type: 'varchar'
  is_nullable: 0
  size: 45

=head2 status

  data_type: 'enum'
  extra: {list => ["DONE","PEND","RUN","FAIL","KILL"]}
  is_nullable: 0

=head2 created

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 0

=head2 opened

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 1

=head2 closed

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 1

=head2 message

  data_type: 'mediumtext'
  is_nullable: 1

=head2 lsf_id

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "data_file",
  { data_type => "mediumtext", is_nullable => 0 },
  "author",
  { data_type => "varchar", is_nullable => 0, size => 45 },
  "uuid",
  { data_type => "varchar", is_nullable => 0, size => 45 },
  "status",
  {
    data_type => "enum",
    extra => { list => ["DONE", "PEND", "RUN", "FAIL", "KILL"] },
    is_nullable => 0,
  },
  "created",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 0,
  },
  "opened",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 1,
  },
  "closed",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 1,
  },
  "message",
  { data_type => "mediumtext", is_nullable => 1 },
  "lsf_id",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-30 15:46:00
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:aHCcK7coSq3DiMxrsnOsCw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
