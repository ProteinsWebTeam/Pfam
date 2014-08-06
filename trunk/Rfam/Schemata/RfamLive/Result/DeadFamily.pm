use utf8;
package RfamDB::Result::DeadFamily;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::DeadFamily

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<dead_family>

=cut

__PACKAGE__->table("dead_family");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  default_value: (empty string)
  is_nullable: 0
  size: 7

record the author???

=head2 rfam_id

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

=head2 title

  data_type: 'varchar'
  is_nullable: 1
  size: 150

wikipedia page title


=head2 user

  data_type: 'tinytext'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", default_value => "", is_nullable => 0, size => 7 },
  "rfam_id",
  { data_type => "varchar", is_nullable => 0, size => 40 },
  "comment",
  { data_type => "mediumtext", is_nullable => 1 },
  "forward_to",
  { data_type => "varchar", is_nullable => 1, size => 7 },
  "title",
  { data_type => "varchar", is_nullable => 1, size => 150 },
  "user",
  { data_type => "tinytext", is_nullable => 0 },
);

=head1 UNIQUE CONSTRAINTS

=head2 C<rfam_acc>

=over 4

=item * L</rfam_acc>

=back

=cut

__PACKAGE__->add_unique_constraint("rfam_acc", ["rfam_acc"]);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-31 15:25:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:CoTJCIastfsgKEHnwddRkg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
