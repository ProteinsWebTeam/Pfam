use utf8;
package RfamDB::Result::Motif;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamDB::Result::Motif

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<motif>

=cut

__PACKAGE__->table("motif");

=head1 ACCESSORS

=head2 motif_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 motif_id

  data_type: 'varchar'
  is_nullable: 1
  size: 40

=head2 description

  data_type: 'varchar'
  is_nullable: 1
  size: 75

=head2 author

  data_type: 'tinytext'
  is_nullable: 1

=head2 seed_source

  data_type: 'tinytext'
  is_nullable: 1

=head2 gathering_cutoff

  data_type: 'double precision'
  is_nullable: 1
  size: [5,2]

=head2 trusted_cutoff

  data_type: 'double precision'
  is_nullable: 1
  size: [5,2]

=head2 noise_cutoff

  data_type: 'double precision'
  is_nullable: 1
  size: [5,2]

=head2 cmbuild

  data_type: 'tinytext'
  is_nullable: 1

=head2 cmcalibrate

  data_type: 'tinytext'
  is_nullable: 1

=head2 type

  data_type: 'varchar'
  is_nullable: 1
  size: 50

=head2 num_seed

  data_type: 'bigint'
  is_nullable: 1
  size: 20

=head2 average_id

  data_type: 'double precision'
  is_nullable: 1
  size: [5,2]

=head2 average_sqlen

  data_type: 'double precision'
  is_nullable: 1
  size: [7,2]

=head2 ecmli_lambda

  data_type: 'double precision'
  is_nullable: 1
  size: [10,5]

=head2 ecmli_mu

  data_type: 'double precision'
  is_nullable: 1
  size: [10,5]

=head2 ecmli_cal_db

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 1

=head2 ecmli_cal_hits

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 1

=head2 maxl

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 1

=head2 clen

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 1

=head2 match_pair_node

  data_type: 'tinyint'
  default_value: 0
  is_nullable: 1

=head2 hmm_tau

  data_type: 'double precision'
  is_nullable: 1
  size: [10,5]

=head2 hmm_lambda

  data_type: 'double precision'
  is_nullable: 1
  size: [10,5]

=head2 wiki

  data_type: 'varchar'
  is nullable: 1
  size: 80

=head2 created

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 0

=head2 updated

  data_type: 'timestamp'
  datetime_undef_if_invalid: 1
  default_value: current_timestamp
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "motif_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "motif_id",
  { data_type => "varchar", is_nullable => 1, size => 40 },
  "description",
  { data_type => "varchar", is_nullable => 1, size => 75 },
  "author",
  { data_type => "tinytext", is_nullable => 1 },
  "seed_source",
  { data_type => "tinytext", is_nullable => 1 },
  "gathering_cutoff",
  { data_type => "double precision", is_nullable => 1, size => [5, 2] },
  "trusted_cutoff",
  { data_type => "double precision", is_nullable => 1, size => [5, 2] },
  "noise_cutoff",
  { data_type => "double precision", is_nullable => 1, size => [5, 2] },
  "cmbuild",
  { data_type => "tinytext", is_nullable => 1 },
  "cmcalibrate",
  { data_type => "tinytext", is_nullable => 1 },
  "type",
  { data_type => "varchar", is_nullable => 1, size => 50 },
  "num_seed",
  { data_type => "bigint", is_nullable => 1, size => 20 },
  "average_id",
  { data_type => "double precision", is_nullable => 1, size => [5, 2] },
  "average_sqlen",
  { data_type => "double precision", is_nullable => 1, size => [7, 2] },
  "ecmli_lambda",
  { data_type => "double precision", is_nullable => 1, size => [10, 5] },
  "ecmli_mu",
  { data_type => "double precision", is_nullable => 1, size => [10, 5] },
  "ecmli_cal_db",
  { data_type => "mediumint", default_value => 0, is_nullable => 1 },
  "ecmli_cal_hits",
  { data_type => "mediumint", default_value => 0, is_nullable => 1 },
  "maxl",
  { data_type => "mediumint", default_value => 0, is_nullable => 1 },
  "clen",
  { data_type => "mediumint", default_value => 0, is_nullable => 1 },
  "match_pair_node",
  { data_type => "tinyint", default_value => 0, is_nullable => 1 },
  "hmm_tau",
  { data_type => "double precision", is_nullable => 1, size => [10, 5] },
  "hmm_lambda",
  { data_type => "double precision", is_nullable => 1, size => [10, 5] },
  "wiki",
  { data_type => "varchar", is_nullable => 1, size => 80 }, 
  "created",
  {
    data_type => "datetime",
    datetime_undef_if_invalid => 1,
    is_nullable => 0,
  },
  "updated",
  {
    data_type => "timestamp",
    datetime_undef_if_invalid => 1,
    default_value => \"current_timestamp",
    is_nullable => 0,
  },
);

=head1 PRIMARY KEY

=over 4

=item * L</motif_acc>

=back

=cut

__PACKAGE__->set_primary_key("motif_acc");

=head1 RELATIONS

=head2 motif_database_links

Type: has_many

Related object: L<RfamDB::Result::MotifDatabaseLink>

=cut

__PACKAGE__->has_many(
  "motif_database_links",
  "RfamDB::Result::MotifDatabaseLink",
  { "foreign.motif_acc" => "self.motif_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


=head2 motif_family_stats

Type: has_many

Related object: L<RfamDB::Result::MotifFamilyStat>

=cut

__PACKAGE__->has_many(
  "motif_family_stats",
  "RfamDB::Result::MotifFamilyStat",
  { "foreign.motif_acc" => "self.motif_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 motif_literatures

Type: has_many

Related object: L<RfamDB::Result::MotifLiterature>

=cut

__PACKAGE__->has_many(
  "motif_literatures",
  "RfamDB::Result::MotifLiterature",
  { "foreign.motif_acc" => "self.motif_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 motif_matches

Type: has_many

Related object: L<RfamDB::Result::MotifMatch>

=cut

__PACKAGE__->has_many(
  "motif_matches",
  "RfamDB::Result::MotifMatch",
  { "foreign.motif_acc" => "self.motif_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 motif_pdbs

Type: has_many

Related object: L<RfamDB::Result::MotifPdb>

=cut

__PACKAGE__->has_many(
  "motif_pdbs",
  "RfamDB::Result::MotifPdb",
  { "foreign.motif_acc" => "self.motif_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-07-02 08:33:40
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:4jdC4SlsvEzuScvqkccEbw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
