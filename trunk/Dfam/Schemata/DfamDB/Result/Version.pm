package DfamDB::Result::Version;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamDB::Result::Version

=cut

__PACKAGE__->table("version");

=head1 ACCESSORS

=head2 dfam_release

  data_type: 'tinytext'
  is_nullable: 1

=head2 dfam_release_date

  data_type: 'date'
  is_nullable: 1

=head2 ensembl_release

  data_type: 'integer'
  is_nullable: 1

=head2 hmmer_version

  data_type: 'tinytext'
  is_nullable: 1

=head2 number_families

  data_type: 'integer'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "dfam_release",
  { data_type => "tinytext", is_nullable => 1 },
  "dfam_release_date",
  { data_type => "date", is_nullable => 1 },
  "ensembl_release",
  { data_type => "integer", is_nullable => 1 },
  "hmmer_version",
  { data_type => "tinytext", is_nullable => 1 },
  "number_families",
  { data_type => "integer", is_nullable => 1 },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-01-11 15:01:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:6LDSYYjV0sWlHzHBEC/F0Q


# You can replace this text with custom content, and it will be preserved on regeneration
1;
