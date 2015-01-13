use utf8;
package PfamLive::Result::PreliminaryPfamB;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PreliminaryPfamB

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<_preliminary_pfamB>

=cut

__PACKAGE__->table("_preliminary_pfamB");

=head1 ACCESSORS

=head2 auto_tmp_pfamb

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 number_archs

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_species

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_structures

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_regions

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_tmp_pfamb",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "number_archs",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "number_species",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "number_structures",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "number_regions",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 1,
  },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_tmp_pfamb>

=back

=cut

__PACKAGE__->set_primary_key("auto_tmp_pfamb");


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:/2BMFpaSz3r+kh+YrcDgew
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PreliminaryPfamB.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::PreliminaryPfamB;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PreliminaryPfamB

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<_preliminary_pfamB>

=cut

__PACKAGE__->table("_preliminary_pfamB");

=head1 ACCESSORS

=head2 auto_tmp_pfamb

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 number_archs

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_species

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_structures

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 number_regions

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_tmp_pfamb",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "number_archs",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "number_species",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "number_structures",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 1 },
  "number_regions",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 1,
  },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_tmp_pfamb>

=back

=cut

__PACKAGE__->set_primary_key("auto_tmp_pfamb");


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3g8CuiPSaywmVqa+wHv3Ww


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PreliminaryPfamB.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
