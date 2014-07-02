use utf8;
package RfamLive::Result::MotifFamilyStat;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::MotifFamilyStat

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<motif_family_stats>

=cut

__PACKAGE__->table("motif_family_stats");

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

=head2 num_hits

  data_type: 'integer'
  is_nullable: 1

=head2 frac_hits

  data_type: 'decimal'
  is_nullable: 1
  size: [4,3]

=head2 sum_bits

  data_type: 'decimal'
  is_nullable: 1
  size: [12,3]

=head2 avg_weight_bits

  data_type: 'decimal'
  is_nullable: 1
  size: [12,3]

=cut

__PACKAGE__->add_columns(
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "motif_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "num_hits",
  { data_type => "integer", is_nullable => 1 },
  "frac_hits",
  { data_type => "decimal", is_nullable => 1, size => [4, 3] },
  "sum_bits",
  { data_type => "decimal", is_nullable => 1, size => [12, 3] },
  "avg_weight_bits",
  { data_type => "decimal", is_nullable => 1, size => [12, 3] },
);

=head1 RELATIONS

=head2 motif_acc

Type: belongs_to

Related object: L<RfamLive::Result::Motif>

=cut

__PACKAGE__->belongs_to(
  "motif_acc",
  "RfamLive::Result::Motif",
  { motif_acc => "motif_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);

=head2 rfam_acc

Type: belongs_to

Related object: L<RfamLive::Result::Family>

=cut

__PACKAGE__->belongs_to(
  "rfam_acc",
  "RfamLive::Result::Family",
  { rfam_acc => "rfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-07-02 08:33:40
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Yd5Be4ND9yzTF7tzaPomYA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
