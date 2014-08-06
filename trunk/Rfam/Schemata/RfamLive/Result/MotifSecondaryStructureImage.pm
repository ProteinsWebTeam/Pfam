use utf8;
package RfamLive::Result::MotifSecondaryStructureImage;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::MotifSecondaryStructureImage

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<motif_ss_image>

=cut

__PACKAGE__->table("motif_ss_image");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 motif_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 image

  data_type: 'longblob'
  is_nullable: 1

=cut


__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },

  "motif_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  
  "image",
  { data_type => "longblob", is_nullable => 1 },
);

=head1 RELATIONS

=head2 rfam_acc

Type: belongs_to

Related object: L<RfamLive::Result::Family>

=cut

__PACKAGE__->belongs_to(
  "rfam_acc",
  "RfamLive::Result::Family",
  { rfam_acc => "rfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 motif_acc

Type: belongs_to

Related object: L<RfamLive::Result::Motif>

=cut

__PACKAGE__->belongs_to(
  "motif_acc",
  "RfamLive::Result::Motif",
  { motif_acc => "motif_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:i3UAdDjdi71gNQEFCifZtw

__PACKAGE__->set_primary_key('rfam_acc','motif_acc');

1;
