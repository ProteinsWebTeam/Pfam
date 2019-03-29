use utf8;
package PfamLive::Result::PfamA;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::PfamA

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA>

=cut

__PACKAGE__->table("pfamA");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  default_value: (empty string)
  is_nullable: 0
  size: 8

=head2 pfama_id

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 previous_id

  data_type: 'tinytext'
  is_nullable: 1

=head2 description

  data_type: 'varchar'
  is_nullable: 0
  size: 100

=head2 deposited_by

  data_type: 'varchar'
  default_value: 'anon'
  is_nullable: 0
  size: 100

=head2 seed_source

  data_type: 'tinytext'
  is_nullable: 0

=head2 type

  data_type: 'varchar'
  is_nullable: 0
  size: 30

=head2 comment

  data_type: 'longtext'
  is_nullable: 1

=head2 sequence_ga

  data_type: 'double precision'
  is_nullable: 0
  size: [8,2]

=head2 domain_ga

  data_type: 'double precision'
  is_nullable: 0
  size: [8,2]

=head2 sequence_tc

  data_type: 'double precision'
  is_nullable: 0
  size: [8,2]

=head2 domain_tc

  data_type: 'double precision'
  is_nullable: 0
  size: [8,2]

=head2 sequence_nc

  data_type: 'double precision'
  is_nullable: 0
  size: [8,2]

=head2 domain_nc

  data_type: 'double precision'
  is_nullable: 0
  size: [8,2]

=head2 buildmethod

  data_type: 'tinytext'
  is_nullable: 0

=head2 model_length

  data_type: 'mediumint'
  is_nullable: 0

=head2 searchmethod

  data_type: 'tinytext'
  is_nullable: 0

=head2 msv_lambda

  data_type: 'double precision'
  is_nullable: 0
  size: [8,2]

=head2 msv_mu

  data_type: 'double precision'
  is_nullable: 0
  size: [8,2]

=head2 viterbi_lambda

  data_type: 'double precision'
  is_nullable: 0
  size: [8,2]

=head2 viterbi_mu

  data_type: 'double precision'
  is_nullable: 0
  size: [8,2]

=head2 forward_lambda

  data_type: 'double precision'
  is_nullable: 0
  size: [8,2]

=head2 forward_tau

  data_type: 'double precision'
  is_nullable: 0
  size: [8,2]

=head2 num_seed

  data_type: 'integer'
  is_nullable: 1

=head2 num_full

  data_type: 'integer'
  is_nullable: 1

=head2 updated

  data_type: 'timestamp'
  datetime_undef_if_invalid: 1
  default_value: current_timestamp
  is_nullable: 0

=head2 created

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 1

=head2 version

  data_type: 'smallint'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", default_value => "", is_nullable => 0, size => 8 },
  "pfama_id",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "previous_id",
  { data_type => "tinytext", is_nullable => 1 },
  "description",
  { data_type => "varchar", is_nullable => 0, size => 100 },
  "deposited_by",
  {
    data_type => "varchar",
    default_value => "anon",
    is_nullable => 0,
    size => 100,
  },
  "seed_source",
  { data_type => "tinytext", is_nullable => 0 },
  "type",
  { data_type => "varchar", is_nullable => 0, size => 30 },
  "comment",
  { data_type => "longtext", is_nullable => 1 },
  "sequence_ga",
  { data_type => "double precision", is_nullable => 0, size => [8, 2] },
  "domain_ga",
  { data_type => "double precision", is_nullable => 0, size => [8, 2] },
  "sequence_tc",
  { data_type => "double precision", is_nullable => 0, size => [8, 2] },
  "domain_tc",
  { data_type => "double precision", is_nullable => 0, size => [8, 2] },
  "sequence_nc",
  { data_type => "double precision", is_nullable => 0, size => [8, 2] },
  "domain_nc",
  { data_type => "double precision", is_nullable => 0, size => [8, 2] },
  "buildmethod",
  { data_type => "tinytext", is_nullable => 0 },
  "model_length",
  { data_type => "mediumint", is_nullable => 0 },
  "searchmethod",
  { data_type => "tinytext", is_nullable => 0 },
  "msv_lambda",
  { data_type => "double precision", is_nullable => 0, size => [8, 2] },
  "msv_mu",
  { data_type => "double precision", is_nullable => 0, size => [8, 2] },
  "viterbi_lambda",
  { data_type => "double precision", is_nullable => 0, size => [8, 2] },
  "viterbi_mu",
  { data_type => "double precision", is_nullable => 0, size => [8, 2] },
  "forward_lambda",
  { data_type => "double precision", is_nullable => 0, size => [8, 2] },
  "forward_tau",
  { data_type => "double precision", is_nullable => 0, size => [8, 2] },
  "num_seed",
  { data_type => "integer", is_nullable => 1 },
  "num_full",
  { data_type => "integer", is_nullable => 1 },
  "updated",
  {
    data_type => "timestamp",
    datetime_undef_if_invalid => 1,
    default_value => \"current_timestamp",
    is_nullable => 0,
  },
  "created",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 1,
  },
  "version",
  { data_type => "smallint", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pfama_acc>

=back

=cut

__PACKAGE__->set_primary_key("pfama_acc");

=head1 UNIQUE CONSTRAINTS

=head2 C<pfamA_id>

=over 4

=item * L</pfama_id>

=back

=cut

__PACKAGE__->add_unique_constraint("pfamA_id", ["pfama_id"]);

=head1 RELATIONS

=head2 pfam_a_hmms

Type: has_many

Related object: L<PfamLive::Result::PfamAHmm>

=cut

__PACKAGE__->has_many(
  "pfam_a_hmms",
  "PfamLive::Result::PfamAHmm",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_reg_full_significants

Type: has_many

Related object: L<PfamLive::Result::PfamARegFullSignificant>

=cut

__PACKAGE__->has_many(
  "pfam_a_reg_full_significants",
  "PfamLive::Result::PfamARegFullSignificant",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_reg_seeds

Type: has_many

Related object: L<PfamLive::Result::PfamARegSeed>

=cut

__PACKAGE__->has_many(
  "pfam_a_reg_seeds",
  "PfamLive::Result::PfamARegSeed",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_seeds

Type: has_many

Related object: L<PfamLive::Result::PfamASeed>

=cut

__PACKAGE__->has_many(
  "pfam_a_seeds",
  "PfamLive::Result::PfamASeed",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2019-03-22 14:44:23
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Rz5qK8uPSjehDVKRWsndlg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
