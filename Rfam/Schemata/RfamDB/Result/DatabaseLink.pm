use utf8;
package RfamDB::Result::DatabaseLink;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::DatabaseLink

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<database_link>

=cut

__PACKAGE__->table("database_link");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 db_id

  data_type: 'tinytext'
  is_nullable: 0

=head2 comment

  data_type: 'tinytext'
  is_nullable: 1

=head2 db_link

  data_type: 'tinytext'
  is_nullable: 0

=head2 other_params

  data_type: 'tinytext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "db_id",
  { data_type => "tinytext", is_nullable => 0 },
  "comment",
  { data_type => "tinytext", is_nullable => 1 },
  "db_link",
  { data_type => "tinytext", is_nullable => 0 },
  "other_params",
  { data_type => "tinytext", is_nullable => 1 },
);

=head1 RELATIONS

=head2 rfam_acc

Type: belongs_to

Related object: L<RfamDB::Result::Family>

=cut

__PACKAGE__->belongs_to(
  "rfam_acc",
  "RfamDB::Result::Family",
  { rfam_acc => "rfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:wXX43BsozVjs3gM+jLtJBA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
