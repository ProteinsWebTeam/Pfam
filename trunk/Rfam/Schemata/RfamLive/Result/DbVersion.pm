use utf8;
package RfamLive::Result::DbVersion;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::DbVersion

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<db_version>

=cut

__PACKAGE__->table("db_version");

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


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-23 13:50:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:jdUGu4tZROhIsO0V5wFdrQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
