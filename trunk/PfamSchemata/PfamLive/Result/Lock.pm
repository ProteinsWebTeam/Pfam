use utf8;
package PfamLive::Result::Lock;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Lock

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<_lock>

=cut

__PACKAGE__->table("_lock");

=head1 ACCESSORS

=head2 locked

  data_type: 'tinyint'
  is_nullable: 0

=head2 locker

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=head2 allowcommits

  data_type: 'tinyint'
  is_nullable: 0

=head2 alsoallow

  data_type: 'text'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "locked",
  { data_type => "tinyint", is_nullable => 0 },
  "locker",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "allowcommits",
  { data_type => "tinyint", is_nullable => 0 },
  "alsoallow",
  { data_type => "text", is_nullable => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:EkrLbQwehplR4g3DEiVk4A
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/Lock.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::Lock;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Lock

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<_lock>

=cut

__PACKAGE__->table("_lock");

=head1 ACCESSORS

=head2 locked

  data_type: 'tinyint'
  is_nullable: 0

=head2 locker

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=head2 allowcommits

  data_type: 'tinyint'
  is_nullable: 0

=head2 alsoallow

  data_type: 'text'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "locked",
  { data_type => "tinyint", is_nullable => 0 },
  "locker",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "allowcommits",
  { data_type => "tinyint", is_nullable => 0 },
  "alsoallow",
  { data_type => "text", is_nullable => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:V4/+7th/dp04faOPYaUz6A


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/Lock.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
