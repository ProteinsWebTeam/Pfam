use utf8;
package PfamLive::Result::Adda;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Adda

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<adda>

=cut

__PACKAGE__->table("adda");

=head1 ACCESSORS

=head2 adda_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 20

=head2 seqacc

  data_type: 'varchar'
  is_nullable: 0
  size: 6

=head2 version

  data_type: 'tinyint'
  is_nullable: 0

=head2 start

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 end

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "adda_acc",
  { data_type => "varchar", is_nullable => 0, size => 20 },
  "seqacc",
  { data_type => "varchar", is_nullable => 0, size => 6 },
  "version",
  { data_type => "tinyint", is_nullable => 0 },
  "start",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 0 },
  "end",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:BjvN2xPlzVPX5rSG/m3k3A
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/Adda.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::Adda;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Adda

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<adda>

=cut

__PACKAGE__->table("adda");

=head1 ACCESSORS

=head2 adda_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 20

=head2 seqacc

  data_type: 'varchar'
  is_nullable: 0
  size: 6

=head2 version

  data_type: 'tinyint'
  is_nullable: 0

=head2 start

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 end

  data_type: 'integer'
  extra: {unsigned => 1}
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "adda_acc",
  { data_type => "varchar", is_nullable => 0, size => 20 },
  "seqacc",
  { data_type => "varchar", is_nullable => 0, size => 6 },
  "version",
  { data_type => "tinyint", is_nullable => 0 },
  "start",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 0 },
  "end",
  { data_type => "integer", extra => { unsigned => 1 }, is_nullable => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:bP24RQcHecj0iW86H4it3g


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/Adda.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
