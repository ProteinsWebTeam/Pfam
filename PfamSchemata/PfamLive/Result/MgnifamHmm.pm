use utf8;
package PfamLive::Result::MgnifamHmm;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::MgnifamHmm

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<mgnifam_HMM>

=cut

__PACKAGE__->table("mgnifam_HMM");

=head1 ACCESSORS

=head2 mgnifam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 9

=head2 hmm

  data_type: 'mediumblob'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "mgnifam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 9 },
  "hmm",
  { data_type => "mediumblob", is_nullable => 1 },
);

=head1 RELATIONS

=head2 mgnifam_acc

Type: belongs_to

Related object: L<PfamLive::Result::Mgnifam>

=cut

__PACKAGE__->belongs_to(
  "mgnifam_acc",
  "PfamLive::Result::Mgnifam",
  { mgnifam_acc => "mgnifam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2019-05-03 13:41:53
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:NVvqx3LMOngI0s9TGhRozw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
