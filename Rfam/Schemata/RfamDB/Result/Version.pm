use utf8;
package RfamDB::Result::Version;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::Version

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<version>

=cut

__PACKAGE__->table("version");

=head1 ACCESSORS

=head2 rfam_release

  data_type: 'double precision'
  is_nullable: 0
  size: [4,1]

=head2 rfam_release_date

  data_type: 'date'
  datetime_undef_if_invalid: 1
  is_nullable: 0

=head2 number_families

  data_type: 'integer'
  is_nullable: 0

=head2 embl_release

  data_type: 'tinytext'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "rfam_release",
  { data_type => "double precision", is_nullable => 0, size => [4, 1] },
  "rfam_release_date",
  { data_type => "date", datetime_undef_if_invalid => 1, is_nullable => 0 },
  "number_families",
  { data_type => "integer", is_nullable => 0 },
  "embl_release",
  { data_type => "tinytext", is_nullable => 0 },
);

=head1 PRIMARY KEY

=over 4

=item * L</rfam_release>

=back

=cut

__PACKAGE__->set_primary_key("rfam_release");


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2014-07-10 16:42:39
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:fUcecb0S4bMH4Epaz74HvQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
