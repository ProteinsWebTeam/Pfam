use utf8;
package RfamDB::Result::PdbRfamReg;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::PdbRfamReg

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb_rfam_reg>

=cut

__PACKAGE__->table("pdb_rfam_reg");

=head1 ACCESSORS

=head2 auto_pdb_reg

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 rfam_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 pdb_seq

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 6

=head2 pdb_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 4

=head2 chain

  data_type: 'varchar'
  default_value: 'NULL'
  is_nullable: 1
  size: 4

=head2 pdb_res_start

  data_type: 'mediumint'
  is_nullable: 1

=head2 pdb_res_end

  data_type: 'mediumint'
  is_nullable: 1

=head2 rfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 20

=head2 seq_start

  data_type: 'bigint'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 seq_end

  data_type: 'bigint'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 hex_colour

  data_type: 'varchar'
  default_value: 'NULL'
  is_nullable: 1
  size: 6

=cut

__PACKAGE__->add_columns(
  "auto_pdb_reg",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "rfam_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "pdb_seq",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 6 },
  "pdb_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 4 },
  "chain",
  {
    data_type => "varchar",
    default_value => "NULL",
    is_nullable => 1,
    size => 4,
  },
  "pdb_res_start",
  { data_type => "mediumint", is_nullable => 1 },
  "pdb_res_end",
  { data_type => "mediumint", is_nullable => 1 },
  "rfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 20 },
  "seq_start",
  { data_type => "bigint", extra => { unsigned => 1 }, is_nullable => 1 },
  "seq_end",
  { data_type => "bigint", extra => { unsigned => 1 }, is_nullable => 1 },
  "hex_colour",
  {
    data_type => "varchar",
    default_value => "NULL",
    is_nullable => 1,
    size => 6,
  },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_pdb_reg>

=back

=cut

__PACKAGE__->set_primary_key("auto_pdb_reg");

=head1 RELATIONS

=head2 pdb

Type: belongs_to

Related object: L<RfamDB::Result::Pdb>

=cut

__PACKAGE__->belongs_to(
  "pdb",
  "RfamDB::Result::Pdb",
  { pdb_id => "pdb_id" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 pdb_seq

Type: belongs_to

Related object: L<RfamDB::Result::PdbSequence>

=cut

__PACKAGE__->belongs_to(
  "pdb_seq",
  "RfamDB::Result::PdbSequence",
  { pdb_seq => "pdb_seq" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 rfam_acc

Type: belongs_to

Related object: L<RfamDB::Result::Family>

=cut

__PACKAGE__->belongs_to(
  "rfam_acc",
  "RfamDB::Result::Family",
  { rfam_acc => "rfam_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 rfamseq_acc

Type: belongs_to

Related object: L<RfamDB::Result::Rfamseq>

=cut

__PACKAGE__->belongs_to(
  "rfamseq_acc",
  "RfamDB::Result::Rfamseq",
  { rfamseq_acc => "rfamseq_acc" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-01-30 15:46:01
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:o/LIyMuVnnyZeUy1YDXnTg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
