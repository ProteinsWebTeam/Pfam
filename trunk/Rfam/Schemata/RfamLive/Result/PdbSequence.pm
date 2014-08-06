use utf8;
package RfamLive::Result::PdbSequence;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::PdbSequence

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb_sequence>

=cut

__PACKAGE__->table("pdb_sequence");

=head1 ACCESSORS

=head2 pdb_seq

  data_type: 'varchar'
  is_nullable: 0
  size: 6

=head2 pdb_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 4

=head2 chain

  data_type: 'varchar'
  is_nullable: 1
  size: 1

=cut

__PACKAGE__->add_columns(
  "pdb_seq",
  { data_type => "varchar", is_nullable => 0, size => 6 },
  "pdb_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 4 },
  "chain",
  { data_type => "varchar", is_nullable => 1, size => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pdb_seq>

=back

=cut

__PACKAGE__->set_primary_key("pdb_seq");

=head1 RELATIONS

=head2 pdb

Type: belongs_to

Related object: L<RfamLive::Result::Pdb>

=cut

__PACKAGE__->belongs_to(
  "pdb",
  "RfamLive::Result::Pdb",
  { pdb_id => "pdb_id" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 pdb_rfam_regs

Type: has_many

Related object: L<RfamLive::Result::PdbRfamReg>

=cut

__PACKAGE__->has_many(
  "pdb_rfam_regs",
  "RfamLive::Result::PdbRfamReg",
  { "foreign.pdb_seq" => "self.pdb_seq" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-29 23:35:51
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:LZdI6lrNtLW+gxmg8jp8jQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
