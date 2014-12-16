use utf8;
package PfamLive::Result::PdbResidueData;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PdbResidueData

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb_residue_data>

=cut

__PACKAGE__->table("pdb_residue_data");

=head1 ACCESSORS

=head2 pdb_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 5

=head2 chain

  data_type: 'varchar'
  is_nullable: 1
  size: 4

=head2 serial

  data_type: 'integer'
  is_nullable: 1

=head2 pdb_res

  data_type: 'char'
  is_nullable: 1
  size: 3

=head2 pdb_seq_number

  data_type: 'integer'
  is_nullable: 1

=head2 pdb_insert_code

  data_type: 'varchar'
  is_nullable: 1
  size: 1

=head2 observed

  data_type: 'integer'
  is_nullable: 1

=head2 dssp_code

  data_type: 'varchar'
  is_nullable: 1
  size: 4

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 pfamseq_res

  data_type: 'char'
  is_nullable: 1
  size: 3

=head2 pfamseq_seq_number

  data_type: 'integer'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 5 },
  "chain",
  { data_type => "varchar", is_nullable => 1, size => 4 },
  "serial",
  { data_type => "integer", is_nullable => 1 },
  "pdb_res",
  { data_type => "char", is_nullable => 1, size => 3 },
  "pdb_seq_number",
  { data_type => "integer", is_nullable => 1 },
  "pdb_insert_code",
  { data_type => "varchar", is_nullable => 1, size => 1 },
  "observed",
  { data_type => "integer", is_nullable => 1 },
  "dssp_code",
  { data_type => "varchar", is_nullable => 1, size => 4 },
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "pfamseq_res",
  { data_type => "char", is_nullable => 1, size => 3 },
  "pfamseq_seq_number",
  { data_type => "integer", is_nullable => 1 },
);

=head1 RELATIONS

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
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:9xe1hFhfREWFjn7vjCqjAw

__PACKAGE__->might_have( pfam_a_reg_full_significants => "PfamLive::Result::PfamARegFullSignificant",
 { "foreign.pfamseq_acc" => "self.pfamseq_acc"});

__PACKAGE__->might_have( pfam_a_reg_seeds => "PfamLive::Result::PfamARegSeed",
 	{ "foreign.pfamseq_acc" => "self.pfamseq_acc"});

# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
