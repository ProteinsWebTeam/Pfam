use utf8;
package PfamLive::Result::Version;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Version

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<version>

=cut

__PACKAGE__->table("version");

=head1 ACCESSORS

=head2 pfam_release

  data_type: 'tinytext'
  is_nullable: 1

=head2 pfam_release_date

  data_type: 'date'
  datetime_undef_if_invalid: 1
  is_nullable: 1

=head2 swiss_prot_version

  data_type: 'tinytext'
  is_nullable: 1

=head2 trembl_version

  data_type: 'tinytext'
  is_nullable: 1

=head2 hmmer_version

  data_type: 'tinytext'
  is_nullable: 1

=head2 pfama_coverage

  data_type: 'float'
  is_nullable: 1
  size: [4,1]

=head2 pfamb_additional_coverage

  data_type: 'float'
  is_nullable: 1
  size: [4,1]

=head2 pfama_residue_coverage

  data_type: 'float'
  is_nullable: 1
  size: [4,1]

=head2 pfamb_additional_residue_coverage

  data_type: 'float'
  is_nullable: 1
  size: [4,1]

=head2 number_families

  data_type: 'integer'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfam_release",
  { data_type => "tinytext", is_nullable => 1 },
  "pfam_release_date",
  { data_type => "date", datetime_undef_if_invalid => 1, is_nullable => 1 },
  "swiss_prot_version",
  { data_type => "tinytext", is_nullable => 1 },
  "trembl_version",
  { data_type => "tinytext", is_nullable => 1 },
  "hmmer_version",
  { data_type => "tinytext", is_nullable => 1 },
  "pfama_coverage",
  { data_type => "float", is_nullable => 1, size => [4, 1] },
  "pfamb_additional_coverage",
  { data_type => "float", is_nullable => 1, size => [4, 1] },
  "pfama_residue_coverage",
  { data_type => "float", is_nullable => 1, size => [4, 1] },
  "pfamb_additional_residue_coverage",
  { data_type => "float", is_nullable => 1, size => [4, 1] },
  "number_families",
  { data_type => "integer", is_nullable => 1 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:OaYXzAdOEmBfOq4XaxFIXg

__PACKAGE__->set_primary_key( qw/pfam_release swiss_prot_version trembl_version hmmer_version/);

# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
