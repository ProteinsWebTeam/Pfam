use utf8;
package PfamLive::Result::MgnifamRegSeed;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::MgnifamRegSeed

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<mgnifam_reg_seed>

=cut

__PACKAGE__->table("mgnifam_reg_seed");

=head1 ACCESSORS

=head2 mgnifam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 9

=head2 pfamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 seq_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 seq_end

  data_type: 'mediumint'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "mgnifam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 9 },
  "pfamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "seq_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "seq_end",
  { data_type => "mediumint", is_nullable => 0 },
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


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2019-04-11 09:45:24
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:raT27SYhos0izcwkujyesA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
