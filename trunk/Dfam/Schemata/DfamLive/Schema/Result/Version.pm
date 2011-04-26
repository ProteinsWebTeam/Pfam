package DfamLive::Schema::Result::Version;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamLive::Schema::Result::Version

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


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-03-13 22:18:33
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:mrWBXpkvfviZuX6OK4kmOg
# These lines were loaded from '/opt/dfam/code/Schemata/DfamLive/Schema/Result/Version.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

package DfamLive::Schema::Result::Version;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamLive::Schema::Result::Version

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


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-03-13 22:12:32
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:VfGSQYvby7ppYii5lnfkEg


# You can replace this text with custom content, and it will be preserved on regeneration
1;
# End of lines loaded from '/opt/dfam/code/Schemata/DfamLive/Schema/Result/Version.pm' 


# You can replace this text with custom content, and it will be preserved on regeneration
1;
