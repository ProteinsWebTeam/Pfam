use utf8;
package PfamLive::Result::PdbImage;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PdbImage

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb_image>

=cut

__PACKAGE__->table("pdb_image");

=head1 ACCESSORS

=head2 pdb_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 5

=head2 pdb_image

  data_type: 'longblob'
  is_nullable: 1

=head2 pdb_image_sml

  data_type: 'mediumblob'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 5 },
  "pdb_image",
  { data_type => "longblob", is_nullable => 1 },
  "pdb_image_sml",
  { data_type => "mediumblob", is_nullable => 1 },
);

=head1 RELATIONS

=head2 pdb

Type: belongs_to

Related object: L<PfamLive::Result::Pdb>

=cut

__PACKAGE__->belongs_to(
  "pdb",
  "PfamLive::Result::Pdb",
  { pdb_id => "pdb_id" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:iLC+HLV6Y08yyF4BU2t1DQ
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PdbImage.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::PdbImage;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PdbImage

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb_image>

=cut

__PACKAGE__->table("pdb_image");

=head1 ACCESSORS

=head2 pdb_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 5

=head2 pdb_image

  data_type: 'longblob'
  is_nullable: 1

=head2 pdb_image_sml

  data_type: 'mediumblob'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 5 },
  "pdb_image",
  { data_type => "longblob", is_nullable => 1 },
  "pdb_image_sml",
  { data_type => "mediumblob", is_nullable => 1 },
);

=head1 RELATIONS

=head2 pdb

Type: belongs_to

Related object: L<PfamLive::Result::Pdb>

=cut

__PACKAGE__->belongs_to(
  "pdb",
  "PfamLive::Result::Pdb",
  { pdb_id => "pdb_id" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:LxQvoBoxn9si+eR7FxzOLA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PdbImage.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
