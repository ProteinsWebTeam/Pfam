use utf8;
package RfamDB::Result::Keyword;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::Keyword

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<keywords>

=cut

__PACKAGE__->table("keywords");

=head1 ACCESSORS

=head2 auto_rfam_keywords

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 rfam_acc

  data_type: 'varchar'
  is_nullable: 1
  size: 7

=head2 rfam_id

  data_type: 'varchar'
  is_nullable: 1
  size: 40

=head2 description

  data_type: 'varchar'
  default_value: 'NULL'
  is_nullable: 1
  size: 100

=head2 rfam_general

  data_type: 'longtext'
  is_nullable: 1

=head2 literature

  data_type: 'longtext'
  is_nullable: 1

=head2 wiki

  data_type: 'longtext'
  is_nullable: 1

=head2 auto_rfam

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "auto_rfam_keywords",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "rfam_acc",
  { data_type => "varchar", is_nullable => 1, size => 7 },
  "rfam_id",
  { data_type => "varchar", is_nullable => 1, size => 40 },
  "description",
  {
    data_type => "varchar",
    default_value => "NULL",
    is_nullable => 1,
    size => 100,
  },
  "rfam_general",
  { data_type => "longtext", is_nullable => 1 },
  "literature",
  { data_type => "longtext", is_nullable => 1 },
  "wiki",
  { data_type => "longtext", is_nullable => 1 },
  "auto_rfam",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 0 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_rfam_keywords>

=back

=cut

__PACKAGE__->set_primary_key("auto_rfam_keywords");


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:UUhz2GXZngM5Ea4fHDndiw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
