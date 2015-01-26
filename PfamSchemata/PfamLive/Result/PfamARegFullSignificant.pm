use utf8;
package PfamLive::Result::PfamARegFullSignificant;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamARegFullSignificant

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_reg_full_significant>

=cut

__PACKAGE__->table("pfamA_reg_full_significant");

=head1 ACCESSORS

=head2 auto_pfama_reg_full

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 pfamseq_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 10

=head2 seq_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 seq_end

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 ali_start

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 ali_end

  data_type: 'mediumint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 model_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 model_end

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 domain_bits_score

  data_type: 'double precision'
  default_value: 0.00
  is_nullable: 0
  size: [8,2]

=head2 domain_evalue_score

  data_type: 'varchar'
  is_nullable: 0
  size: 15

=head2 sequence_bits_score

  data_type: 'double precision'
  default_value: 0.00
  is_nullable: 0
  size: [8,2]

=head2 sequence_evalue_score

  data_type: 'varchar'
  is_nullable: 0
  size: 15

=head2 cigar

  data_type: 'text'
  is_nullable: 1

=head2 in_full

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 0

=head2 tree_order

  data_type: 'mediumint'
  is_nullable: 1

=head2 domain_order

  data_type: 'mediumint'
  is_nullable: 1

=head2 domain_oder

  data_type: 'tinyint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_pfama_reg_full",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "pfamseq_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 10 },
  "seq_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "seq_end",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "ali_start",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 0 },
  "ali_end",
  { data_type => "mediumint", extra => { unsigned => 1 }, is_nullable => 0 },
  "model_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "model_end",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "domain_bits_score",
  {
    data_type => "double precision",
    default_value => "0.00",
    is_nullable => 0,
    size => [8, 2],
  },
  "domain_evalue_score",
  { data_type => "varchar", is_nullable => 0, size => 15 },
  "sequence_bits_score",
  {
    data_type => "double precision",
    default_value => "0.00",
    is_nullable => 0,
    size => [8, 2],
  },
  "sequence_evalue_score",
  { data_type => "varchar", is_nullable => 0, size => 15 },
  "cigar",
  { data_type => "text", is_nullable => 1 },
  "in_full",
  { data_type => "tinyint", default_value => 0, is_nullable => 0 },
  "tree_order",
  { data_type => "mediumint", is_nullable => 1 },
  "domain_order",
  { data_type => "mediumint", is_nullable => 1 },
  "domain_oder",
  { data_type => "tinyint", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_pfama_reg_full>

=back

=cut

__PACKAGE__->set_primary_key("auto_pfama_reg_full");

=head1 RELATIONS

=head2 pdb_pfam_a_regs

Type: has_many

Related object: L<PfamLive::Result::PdbPfamAReg>

=cut

__PACKAGE__->has_many(
  "pdb_pfam_a_regs",
  "PfamLive::Result::PdbPfamAReg",
  { "foreign.auto_pfama_reg_full" => "self.auto_pfama_reg_full" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamLive::Result::PfamA>

=cut

__PACKAGE__->belongs_to(
  "pfama_acc",
  "PfamLive::Result::PfamA",
  { pfama_acc => "pfama_acc" },
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
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:LDMV0uCiWmzSQ9+eaL+cpA
__PACKAGE__->might_have(
	"clan_membership" => 'PfamLive::Result::ClanMembership',
 	{ 'foreign.pfama_acc' => 'self.pfama_acc' } );

__PACKAGE__->might_have(
	"interactions" => 'PfamLive::Result::PfamAInteraction',
	{ 'foreign.pfama_acc_a' => 'self.pfama_acc' } );

# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
