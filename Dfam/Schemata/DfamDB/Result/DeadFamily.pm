package DfamDB::Result::DeadFamily;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamDB::Result::DeadFamily

=cut

__PACKAGE__->table("dead_families");

=head1 ACCESSORS

=head2 dfam_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 dfam_id

  data_type: 'varchar'
  is_nullable: 0
  size: 40

=head2 comment

  data_type: 'mediumtext'
  is_nullable: 1

=head2 forward_to

  data_type: 'varchar'
  is_nullable: 1
  size: 7

=head2 user

  data_type: 'varchar'
  default_value: 'anon'
  is_nullable: 0
  size: 10

=head2 killed

  data_type: 'timestamp'
  default_value: current_timestamp
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "dfam_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "dfam_id",
  { data_type => "varchar", is_nullable => 0, size => 40 },
  "comment",
  { data_type => "mediumtext", is_nullable => 1 },
  "forward_to",
  { data_type => "varchar", is_nullable => 1, size => 7 },
  "user",
  {
    data_type => "varchar",
    default_value => "anon",
    is_nullable => 0,
    size => 10,
  },
  "killed",
  {
    data_type     => "timestamp",
    default_value => \"current_timestamp",
    is_nullable   => 0,
  },
);
__PACKAGE__->add_unique_constraint("dfam_acc", ["dfam_acc"]);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-01-11 15:01:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:RIpelrNmstQsVCcI/wHRxA


# You can replace this text with custom content, and it will be preserved on regeneration
1;
