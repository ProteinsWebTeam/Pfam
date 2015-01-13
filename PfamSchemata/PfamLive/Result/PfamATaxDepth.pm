use utf8;
package PfamLive::Result::PfamATaxDepth;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamATaxDepth

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_tax_depth>

=cut

__PACKAGE__->table("pfamA_tax_depth");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 root

  data_type: 'char'
  is_nullable: 0
  size: 24

=head2 count

  data_type: 'integer'
  is_nullable: 0

=head2 common

  data_type: 'text'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "root",
  { data_type => "char", is_nullable => 0, size => 24 },
  "count",
  { data_type => "integer", is_nullable => 0 },
  "common",
  { data_type => "text", is_nullable => 0 },
);

=head1 RELATIONS

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamLive::Result::PfamA>

=cut

__PACKAGE__->belongs_to(
  "pfama_acc",
  "PfamLive::Result::PfamA",
  { pfama_acc => "pfama_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:7SY2hyptJXSuhlsPlzqBkQ
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PfamATaxDepth.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::PfamATaxDepth;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamATaxDepth

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_tax_depth>

=cut

__PACKAGE__->table("pfamA_tax_depth");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 root

  data_type: 'char'
  is_nullable: 0
  size: 24

=head2 count

  data_type: 'integer'
  is_nullable: 0

=head2 common

  data_type: 'text'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "root",
  { data_type => "char", is_nullable => 0, size => 24 },
  "count",
  { data_type => "integer", is_nullable => 0 },
  "common",
  { data_type => "text", is_nullable => 0 },
);

=head1 RELATIONS

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamLive::Result::PfamA>

=cut

__PACKAGE__->belongs_to(
  "pfama_acc",
  "PfamLive::Result::PfamA",
  { pfama_acc => "pfama_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:phl+S4Jyyu3GBhgWWyWbtQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PfamATaxDepth.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
