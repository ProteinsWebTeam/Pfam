use utf8;
package PfamLive::Result::Mgnifam;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamLive::Result::Mgnifam

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<mgnifam>

=cut

__PACKAGE__->table("mgnifam");

=head1 ACCESSORS

=head2 mgnifam_acc

  data_type: 'varchar'
  default_value: (empty string)
  is_nullable: 0
  size: 9

=head2 mgnifam_id

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

=head2 author

  data_type: 'tinytext'
  is_nullable: 1

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
  default_value: 'CURRENT_TIMESTAMP'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "mgnifam_acc",
  { data_type => "varchar", default_value => "", is_nullable => 0, size => 9 },
  "mgnifam_id",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "previous_id",
  { data_type => "tinytext", is_nullable => 1 },
  "description",
  { data_type => "varchar", is_nullable => 0, size => 100 },
  "author",
  { data_type => "tinytext", is_nullable => 1 },
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
    default_value => "CURRENT_TIMESTAMP",
    is_nullable => 0,
  },
);

=head1 PRIMARY KEY

=over 4

=item * L</mgnifam_acc>

=back

=cut

__PACKAGE__->set_primary_key("mgnifam_acc");

=head1 UNIQUE CONSTRAINTS

=head2 C<mgnifam_id>

=over 4

=item * L</mgnifam_id>

=back

=cut

__PACKAGE__->add_unique_constraint("mgnifam_id", ["mgnifam_id"]);

=head1 RELATIONS

=head2 mgnifam_hmms

Type: has_many

Related object: L<PfamLive::Result::MgnifamHmm>

=cut

__PACKAGE__->has_many(
  "mgnifam_hmms",
  "PfamLive::Result::MgnifamHmm",
  { "foreign.mgnifam_acc" => "self.mgnifam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 mgnifam_reg_fulls

Type: has_many

Related object: L<PfamLive::Result::MgnifamRegFull>

=cut

__PACKAGE__->has_many(
  "mgnifam_reg_fulls",
  "PfamLive::Result::MgnifamRegFull",
  { "foreign.mgnifam_acc" => "self.mgnifam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 mgnifam_reg_seeds

Type: has_many

Related object: L<PfamLive::Result::MgnifamRegSeed>

=cut

__PACKAGE__->has_many(
  "mgnifam_reg_seeds",
  "PfamLive::Result::MgnifamRegSeed",
  { "foreign.mgnifam_acc" => "self.mgnifam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 mgnifam_seeds

Type: has_many

Related object: L<PfamLive::Result::MgnifamSeed>

=cut

__PACKAGE__->has_many(
  "mgnifam_seeds",
  "PfamLive::Result::MgnifamSeed",
  { "foreign.mgnifam_acc" => "self.mgnifam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07046 @ 2019-05-03 13:41:53
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:38VH0bdRb8J2+NR0SjaRWg


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
