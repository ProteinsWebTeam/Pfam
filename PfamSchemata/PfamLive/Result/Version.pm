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


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:u9vmlurKv0buIvuZXN4pGQ
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/Version.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

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


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/Version.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
