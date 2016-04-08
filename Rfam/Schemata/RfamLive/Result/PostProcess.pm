use utf8;
package RfamLive::Result::PostProcess;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::PostProcess

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<_post_process>

=cut

__PACKAGE__->table("_post_process");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

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
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
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

=head1 RELATIONS

=head2 rfam_acc

Type: belongs_to

Related object: L<RfamLive::Result::Family>

=cut

__PACKAGE__->belongs_to(
  "rfam_acc",
  "RfamLive::Result::Family",
  { rfam_acc => "rfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-31 15:25:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:/sf4M5HO/saEtituoQzNaA

__PACKAGE__->set_primary_key('rfam_acc', 'uuid');

sub start {
  my $self = shift;
  $self->update({ status => 'RUN',
                  uuid   => $self->uuid,
                  opened => \'NOW()' });
}


sub run {
  my $self = shift;
   $self->update( {
     status  => 'RUN',
     opened => \'NOW()'
   } );
}

sub done {
  my $self = shift;
  $self->update( {
    status  => 'DONE',
    closed => \'NOW()'
  } );
}

sub failure {
  my $self = shift;
  $self->update( {
    status  => 'FAIL',
    closed => \'NOW()'
  } );
}

1;
