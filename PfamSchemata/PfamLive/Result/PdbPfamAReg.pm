use utf8;
package PfamLive::Result::PdbPfamAReg;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PdbPfamAReg

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb_pfamA_reg>

=cut

__PACKAGE__->table("pdb_pfamA_reg");

=head1 ACCESSORS

=head2 auto_pdb_reg

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 auto_uniprot_reg_full

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 pdb_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 5

=head2 pfama_acc

  data_type: 'varchar'
  default_value: 0
  is_nullable: 0
  size: 7

=head2 pfamseq_acc

  data_type: 'varchar'
  default_value: 0
  is_nullable: 0
  size: 10

=head2 chain

  data_type: 'varchar'
  is_nullable: 1
  size: 4

=head2 pdb_res_start

  data_type: 'mediumint'
  is_nullable: 1

=head2 pdb_start_icode

  data_type: 'varchar'
  is_nullable: 1
  size: 1

=head2 pdb_res_end

  data_type: 'mediumint'
  is_nullable: 1

=head2 pdb_end_icode

  data_type: 'varchar'
  is_nullable: 1
  size: 1

=head2 seq_start

  data_type: 'mediumint'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 seq_end

  data_type: 'mediumint'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 hex_colour

  data_type: 'varchar'
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
  "auto_uniprot_reg_full",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "pdb_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 5 },
  "pfama_acc",
  { data_type => "varchar", default_value => 0, is_nullable => 0, size => 7 },
  "pfamseq_acc",
  { data_type => "varchar", default_value => 0, is_nullable => 0, size => 10 },
  "chain",
  { data_type => "varchar", is_nullable => 1, size => 4 },
  "pdb_res_start",
  { data_type => "mediumint", is_nullable => 1 },
  "pdb_start_icode",
  { data_type => "varchar", is_nullable => 1, size => 1 },
  "pdb_res_end",
  { data_type => "mediumint", is_nullable => 1 },
  "pdb_end_icode",
  { data_type => "varchar", is_nullable => 1, size => 1 },
  "seq_start",
  {
    data_type => "mediumint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "seq_end",
  {
    data_type => "mediumint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "hex_colour",
  { data_type => "varchar", is_nullable => 1, size => 6 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_pdb_reg>

=back

=cut

__PACKAGE__->set_primary_key("auto_pdb_reg");

=head1 RELATIONS

=head2 auto_uniprot_reg_full

Type: belongs_to

Related object: L<PfamLive::Result::UniprotRegFull>

=cut

__PACKAGE__->belongs_to(
  "auto_uniprot_reg_full",
  "PfamLive::Result::UniprotRegFull",
  { auto_uniprot_reg_full => "auto_uniprot_reg_full" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);

=head2 pdb

Type: belongs_to

Related object: L<PfamLive::Result::Pdb>

=cut

__PACKAGE__->belongs_to(
  "pdb",
  "PfamLive::Result::Pdb",
  { pdb_id => "pdb_id" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-09-22 11:24:24
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:YbY5v6CrgD+Bpyjqwc94Lg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
