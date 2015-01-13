use utf8;
package PfamLive::Result::ProteomeArchitecture;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::ProteomeArchitecture

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<proteome_architecture>

=cut

__PACKAGE__->table("proteome_architecture");

=head1 ACCESSORS

=head2 auto_proteome

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 auto_architecture

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 type_example

  data_type: 'varchar'
  default_value: 0
  is_nullable: 0
  size: 10

=head2 no_seqs

  data_type: 'integer'
  default_value: 0
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "auto_proteome",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "auto_architecture",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "type_example",
  { data_type => "varchar", default_value => 0, is_nullable => 0, size => 10 },
  "no_seqs",
  { data_type => "integer", default_value => 0, is_nullable => 0 },
);

=head1 RELATIONS

=head2 auto_architecture

Type: belongs_to

Related object: L<PfamLive::Result::Architecture>

=cut

__PACKAGE__->belongs_to(
  "auto_architecture",
  "PfamLive::Result::Architecture",
  { auto_architecture => "auto_architecture" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 auto_proteome

Type: belongs_to

Related object: L<PfamLive::Result::CompleteProteome>

=cut

__PACKAGE__->belongs_to(
  "auto_proteome",
  "PfamLive::Result::CompleteProteome",
  { auto_proteome => "auto_proteome" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:1oE0C1e0EphRr6E2d77rqQ
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/ProteomeArchitecture.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::ProteomeArchitecture;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::ProteomeArchitecture

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<proteome_architecture>

=cut

__PACKAGE__->table("proteome_architecture");

=head1 ACCESSORS

=head2 auto_proteome

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 auto_architecture

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 type_example

  data_type: 'varchar'
  default_value: 0
  is_nullable: 0
  size: 10

=head2 no_seqs

  data_type: 'integer'
  default_value: 0
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "auto_proteome",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "auto_architecture",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "type_example",
  { data_type => "varchar", default_value => 0, is_nullable => 0, size => 10 },
  "no_seqs",
  { data_type => "integer", default_value => 0, is_nullable => 0 },
);

=head1 RELATIONS

=head2 auto_architecture

Type: belongs_to

Related object: L<PfamLive::Result::Architecture>

=cut

__PACKAGE__->belongs_to(
  "auto_architecture",
  "PfamLive::Result::Architecture",
  { auto_architecture => "auto_architecture" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 auto_proteome

Type: belongs_to

Related object: L<PfamLive::Result::CompleteProteome>

=cut

__PACKAGE__->belongs_to(
  "auto_proteome",
  "PfamLive::Result::CompleteProteome",
  { auto_proteome => "auto_proteome" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:vHlcyyEK1R1UacVj6OP9SA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/ProteomeArchitecture.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
