use utf8;
package PfamLive::Result::DeadFamily;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::DeadFamily

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<dead_family>

=cut

__PACKAGE__->table("dead_family");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 pfama_id

  data_type: 'varchar'
  is_nullable: 0
  size: 40

=head2 comment

  data_type: 'mediumtext'
  is_nullable: 1

=head2 forward_to

  data_type: 'varchar'
  is_nullable: 1
  size: 7

=head2 user

  data_type: 'varchar'
  default_value: 'anon'
  is_nullable: 0
  size: 10

=head2 killed

  data_type: 'timestamp'
  datetime_undef_if_invalid: 1
  default_value: current_timestamp
  is_nullable: 0

=head2 title

  data_type: 'tinytext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "pfama_id",
  { data_type => "varchar", is_nullable => 0, size => 40 },
  "comment",
  { data_type => "mediumtext", is_nullable => 1 },
  "forward_to",
  { data_type => "varchar", is_nullable => 1, size => 7 },
  "user",
  {
    data_type => "varchar",
    default_value => "anon",
    is_nullable => 0,
    size => 10,
  },
  "killed",
  {
    data_type => "timestamp",
    datetime_undef_if_invalid => 1,
    default_value => \"current_timestamp",
    is_nullable => 0,
  },
  "title",
  { data_type => "tinytext", is_nullable => 1 },
);

=head1 UNIQUE CONSTRAINTS

=head2 C<pfamA_acc>

=over 4

=item * L</pfama_acc>

=back

=cut

__PACKAGE__->add_unique_constraint("pfamA_acc", ["pfama_acc"]);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:W9BzOWD4WB1vsWJ9/+EZew
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/DeadFamily.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::DeadFamily;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::DeadFamily

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<dead_family>

=cut

__PACKAGE__->table("dead_family");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 pfama_id

  data_type: 'varchar'
  is_nullable: 0
  size: 40

=head2 comment

  data_type: 'mediumtext'
  is_nullable: 1

=head2 forward_to

  data_type: 'varchar'
  is_nullable: 1
  size: 7

=head2 user

  data_type: 'varchar'
  default_value: 'anon'
  is_nullable: 0
  size: 10

=head2 killed

  data_type: 'timestamp'
  datetime_undef_if_invalid: 1
  default_value: current_timestamp
  is_nullable: 0

=head2 title

  data_type: 'tinytext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "pfama_id",
  { data_type => "varchar", is_nullable => 0, size => 40 },
  "comment",
  { data_type => "mediumtext", is_nullable => 1 },
  "forward_to",
  { data_type => "varchar", is_nullable => 1, size => 7 },
  "user",
  {
    data_type => "varchar",
    default_value => "anon",
    is_nullable => 0,
    size => 10,
  },
  "killed",
  {
    data_type => "timestamp",
    datetime_undef_if_invalid => 1,
    default_value => \"current_timestamp",
    is_nullable => 0,
  },
  "title",
  { data_type => "tinytext", is_nullable => 1 },
);

=head1 UNIQUE CONSTRAINTS

=head2 C<pfamA_acc>

=over 4

=item * L</pfama_acc>

=back

=cut

__PACKAGE__->add_unique_constraint("pfamA_acc", ["pfama_acc"]);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:k/drOE1CSyrNxLMGuO1p7A


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/DeadFamily.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
