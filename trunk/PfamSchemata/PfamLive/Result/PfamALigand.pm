use utf8;
package PfamLive::Result::PfamALigand;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamALigand

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_ligand>

=cut

__PACKAGE__->table("pfamA_ligand");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 ligand_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 3

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "ligand_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 3 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pfama_acc>

=item * L</ligand_id>

=back

=cut

__PACKAGE__->set_primary_key("pfama_acc", "ligand_id");

=head1 RELATIONS

=head2 ligand

Type: belongs_to

Related object: L<PfamLive::Result::Ligand>

=cut

__PACKAGE__->belongs_to(
  "ligand",
  "PfamLive::Result::Ligand",
  { ligand_id => "ligand_id" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

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
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ie3PYhqqySJgnAonPZ98ZQ
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PfamALigand.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

use utf8;
package PfamLive::Result::PfamALigand;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamALigand

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_ligand>

=cut

__PACKAGE__->table("pfamA_ligand");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 ligand_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 3

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "ligand_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 3 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pfama_acc>

=item * L</ligand_id>

=back

=cut

__PACKAGE__->set_primary_key("pfama_acc", "ligand_id");

=head1 RELATIONS

=head2 ligand

Type: belongs_to

Related object: L<PfamLive::Result::Ligand>

=cut

__PACKAGE__->belongs_to(
  "ligand",
  "PfamLive::Result::Ligand",
  { ligand_id => "ligand_id" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

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


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-09-22 17:06:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:fCdY1YyBo7QBuhw1Bf3lwA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PfamALigand.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
