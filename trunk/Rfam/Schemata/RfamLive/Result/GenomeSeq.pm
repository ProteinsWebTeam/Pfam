use utf8;
package RfamDB::Result::GenomeSeq;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::GenomeSeq

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<genome_seq>

=cut

__PACKAGE__->table("genome_seq");

=head1 ACCESSORS

=head2 genome_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 20

=head2 seq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 14

=cut

__PACKAGE__->add_columns(
  "genome_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 20 },
  "seq_acc",
  { data_type => "varchar", is_nullable => 0, size => 14 },
);

=head1 PRIMARY KEY

=over 4

=item * L</seq_acc>

=back

=cut

__PACKAGE__->set_primary_key("seq_acc");

=head1 RELATIONS

=head2 genome_acc

Type: belongs_to

Related object: L<RfamDB::Result::Genome>

=cut

__PACKAGE__->belongs_to(
  "genome_acc",
  "RfamDB::Result::Genome",
  { genome_acc => "genome_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 genome_full_regions

Type: has_many

Related object: L<RfamDB::Result::GenomeFullRegion>

=cut

__PACKAGE__->has_many(
  "genome_full_regions",
  "RfamDB::Result::GenomeFullRegion",
  { "foreign.seq_acc" => "self.seq_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:uZZYf3masvEDtSVF7O+NGg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
