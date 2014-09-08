use utf8;
package PfamLive::Result::SecondaryPfamseqAcc;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::SecondaryPfamseqAcc

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<secondary_pfamseq_acc>

=cut

__PACKAGE__->table("secondary_pfamseq_acc");

=head1 ACCESSORS

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 secondary_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 6

=cut

__PACKAGE__->add_columns(
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "secondary_acc",
  { data_type => "varchar", is_nullable => 0, size => 6 },
);

=head1 RELATIONS

=head2 pfamseq_acc

Type: belongs_to

Related object: L<PfamLive::Result::Pfamseq>

=cut

__PACKAGE__->belongs_to(
  "pfamseq_acc",
  "PfamLive::Result::Pfamseq",
  { pfamseq_acc => "pfamseq_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-05-19 08:45:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ksemB8qQd2eHX+pVRIno0g


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
