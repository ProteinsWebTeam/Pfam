use utf8;
package PfamLive::Result::CurrentPfamVersion;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::CurrentPfamVersion

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<current_pfam_version>

=cut

__PACKAGE__->table("current_pfam_version");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 seed

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=head2 align

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=head2 desc_file

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=head2 hmm

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "seed",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "align",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "desc_file",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "hmm",
  { data_type => "varchar", is_nullable => 0, size => 32 },
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
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:NabLTvu3RdGMCkGrQGEnVw
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/CurrentPfamVersion.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::CurrentPfamVersion;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::CurrentPfamVersion

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<current_pfam_version>

=cut

__PACKAGE__->table("current_pfam_version");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 seed

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=head2 align

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=head2 desc_file

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=head2 hmm

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "seed",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "align",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "desc_file",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "hmm",
  { data_type => "varchar", is_nullable => 0, size => 32 },
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
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:7R0/zPDFyJK6Vw22i1iS8w

__PACKAGE__->set_primary_key("pfama_acc");
__PACKAGE__->add_unique_constraint("pfamA_acc", ["pfama_acc"]);

# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/CurrentPfamVersion.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
