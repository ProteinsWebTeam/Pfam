use utf8;
package PfamLive::Result::PfamARegSeed;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamARegSeed

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_reg_seed>

=cut

__PACKAGE__->table("pfamA_reg_seed");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 pfamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=head2 seq_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 seq_end

  data_type: 'mediumint'
  is_nullable: 0

=head2 cigar

  data_type: 'text'
  is_nullable: 1

=head2 tree_order

  data_type: 'mediumint'
  is_nullable: 1

=head2 seq_version

  data_type: 'tinyint'
  is_nullable: 0

=head2 md5

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=head2 source

  data_type: 'enum'
  extra: {list => ["pfamseq","uniprot"]}
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "pfamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "seq_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "seq_end",
  { data_type => "mediumint", is_nullable => 0 },
  "cigar",
  { data_type => "text", is_nullable => 1 },
  "tree_order",
  { data_type => "mediumint", is_nullable => 1 },
  "seq_version",
  { data_type => "tinyint", is_nullable => 0 },
  "md5",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "source",
  {
    data_type => "enum",
    extra => { list => ["pfamseq", "uniprot"] },
    is_nullable => 0,
  },
);

=head1 RELATIONS

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


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-07-08 11:55:45
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:qXKKYRJ4Dc/yRX/9bo64ow

__PACKAGE__->add_unique_constraint(
 	"pfamA_reg_seed_reg_idx",
 	["pfama_acc", "pfamseq_acc", "seq_start", "seq_end"],
 	);

__PACKAGE__->set_primary_key("pfama_acc", "pfamseq_acc", "seq_start", "seq_end");

__PACKAGE__->might_have(
	"clan_membership" => 'PfamLive::Result::ClanMembership',
	{ 'foreign.pfama_acc' => 'self.pfama_acc' } );

# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
