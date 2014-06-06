use utf8;
package RfamLive::Result::MotifMatch;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::MotifMatch

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<motif_matches>

=cut

__PACKAGE__->table("motif_matches");

=head1 ACCESSORS

=head2 motif_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 rfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 20

=head2 rfamseq_start

  data_type: 'bigint'
  is_nullable: 0

=head2 rfamseq_stop

  data_type: 'bigint'
  is_nullable: 0

=head2 query_start

  data_type: 'integer'
  is_nullable: 1

=head2 query_stop

  data_type: 'integer'
  is_nullable: 1

=head2 motif_start

  data_type: 'integer'
  is_nullable: 1

=head2 motif_stop

  data_type: 'integer'
  is_nullable: 1

=head2 e_value

  data_type: 'varchar'
  is_nullable: 1
  size: 15

=head2 bit_score

  data_type: 'double precision'
  is_nullable: 1
  size: [7,2]

=cut

__PACKAGE__->add_columns(
  "motif_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "rfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 20 },
  "rfamseq_start",
  { data_type => "bigint", is_nullable => 0 },
  "rfamseq_stop",
  { data_type => "bigint", is_nullable => 0 },
  "query_start",
  { data_type => "integer", is_nullable => 1 },
  "query_stop",
  { data_type => "integer", is_nullable => 1 },
  "motif_start",
  { data_type => "integer", is_nullable => 1 },
  "motif_stop",
  { data_type => "integer", is_nullable => 1 },
  "e_value",
  { data_type => "varchar", is_nullable => 1, size => 15 },
  "bit_score",
  { data_type => "double precision", is_nullable => 1, size => [7, 2] },
);

=head1 PRIMARY KEY

=over 4

=item * L</motif_acc>

=item * L</rfam_acc>

=item * L</rfamseq_acc>

=item * L</rfamseq_start>

=item * L</rfamseq_stop>

=back

=cut

__PACKAGE__->set_primary_key(
  "motif_acc",
  "rfam_acc",
  "rfamseq_acc",
  "rfamseq_start",
  "rfamseq_stop",
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

=head2 rfamseq_acc

Type: belongs_to

Related object: L<RfamLive::Result::SeedRegion>

=cut

__PACKAGE__->belongs_to(
  "rfamseq_acc",
  "RfamLive::Result::SeedRegion",
  { rfamseq_acc => "rfamseq_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2014-05-21 14:32:18
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:r7AMhXbxNPWwjEu7yC7vQw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
