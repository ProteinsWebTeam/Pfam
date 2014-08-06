use utf8;
package RfamDB::Result::DbVersion;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::DbVersion

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

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 0

=head2 number_families

  data_type: 'integer'
  is_nullable: 0

=head2 embl_release

  data_type: 'tinytext'
  is_nullable: 0

=head2 genome_collection_date

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 1

=head2 refseq_version

  data_type: 'integer'
  is_nullable: 1

=head2 pdb_date

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 1

=head2 infernal_version

  data_type: 'varchar'
  is_nullable: 1
  size: 45

=cut

__PACKAGE__->add_columns(
  "rfam_release",
  { data_type => "double precision", is_nullable => 0, size => [4, 1] },
  "rfam_release_date",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 0,
  },
  "number_families",
  { data_type => "integer", is_nullable => 0 },
  "embl_release",
  { data_type => "tinytext", is_nullable => 0 },
  "genome_collection_date",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 1,
  },
  "refseq_version",
  { data_type => "integer", is_nullable => 1 },
  "pdb_date",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 1,
  },
  "infernal_version",
  { data_type => "varchar", is_nullable => 1, size => 45 },
);

=head1 PRIMARY KEY

=over 4

=item * L</rfam_release>

=back

=cut

__PACKAGE__->set_primary_key("rfam_release");


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:XKkaMTHIQ9FARGiiTJbCYw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
