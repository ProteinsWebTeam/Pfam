package DfamDB::Result::Dfam;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

DfamDB::Result::Dfam

=cut

__PACKAGE__->table("dfam");

=head1 ACCESSORS

=head2 dfam_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 dfam_id

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
  is_nullable: 0

=head2 deposited_by

  data_type: 'varchar'
  default_value: 'anon'
  is_nullable: 0
  size: 100

=head2 seed_source

  data_type: 'tinytext'
  is_nullable: 0

=head2 type

  data_type: 'enum'
  extra: {list => ["Repeat","Motif"]}
  is_nullable: 0

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
  default_value: current_timestamp
  is_nullable: 0

=head2 created

  data_type: 'datetime'
  is_nullable: 1

=head2 version

  data_type: 'smallint'
  is_nullable: 1

=head2 number_species

  data_type: 'integer'
  is_nullable: 1

=head2 average_length

  data_type: 'double precision'
  is_nullable: 1
  size: [6,2]

=head2 percentage_id

  data_type: 'integer'
  is_nullable: 1

=head2 change_status

  data_type: 'tinytext'
  is_nullable: 1

=head2 seed_consensus

  data_type: 'text'
  is_nullable: 1

=head2 full_consensus

  data_type: 'text'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "dfam_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "dfam_id",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "previous_id",
  { data_type => "tinytext", is_nullable => 1 },
  "description",
  { data_type => "varchar", is_nullable => 0, size => 100 },
  "author",
  { data_type => "tinytext", is_nullable => 0 },
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
  {
    data_type => "enum",
    extra => { list => ["Repeat", "Motif"] },
    is_nullable => 0,
  },
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
    data_type     => "timestamp",
    default_value => \"current_timestamp",
    is_nullable   => 0,
  },
  "created",
  { data_type => "datetime", is_nullable => 1 },
  "version",
  { data_type => "smallint", is_nullable => 1 },
  "number_species",
  { data_type => "integer", is_nullable => 1 },
  "average_length",
  { data_type => "double precision", is_nullable => 1, size => [6, 2] },
  "percentage_id",
  { data_type => "integer", is_nullable => 1 },
  "change_status",
  { data_type => "tinytext", is_nullable => 1 },
  "seed_consensus",
  { data_type => "text", is_nullable => 1 },
  "full_consensus",
  { data_type => "text", is_nullable => 1 },
);
__PACKAGE__->set_primary_key("dfam_acc");
__PACKAGE__->add_unique_constraint("dfam_id", ["dfam_id"]);

=head1 RELATIONS

=head2 dfam_literature_references

Type: has_many

Related object: L<DfamDB::Result::DfamLiteratureReference>

=cut

__PACKAGE__->has_many(
  "dfam_literature_references",
  "DfamDB::Result::DfamLiteratureReference",
  { "foreign.dfam_acc" => "self.dfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 dfam_reg_full_insignificants

Type: has_many

Related object: L<DfamDB::Result::DfamRegFullInsignificant>

=cut

__PACKAGE__->has_many(
  "dfam_reg_full_insignificants",
  "DfamDB::Result::DfamRegFullInsignificant",
  { "foreign.dfam_acc" => "self.dfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 dfam_reg_full_significants

Type: has_many

Related object: L<DfamDB::Result::DfamRegFullSignificant>

=cut

__PACKAGE__->has_many(
  "dfam_reg_full_significants",
  "DfamDB::Result::DfamRegFullSignificant",
  { "foreign.dfam_acc" => "self.dfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 dfam_reg_seeds

Type: has_many

Related object: L<DfamDB::Result::DfamRegSeed>

=cut

__PACKAGE__->has_many(
  "dfam_reg_seeds",
  "DfamDB::Result::DfamRegSeed",
  { "foreign.dfam_acc" => "self.dfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 dfam_wikis

Type: has_many

Related object: L<DfamDB::Result::DfamWiki>

=cut

__PACKAGE__->has_many(
  "dfam_wikis",
  "DfamDB::Result::DfamWiki",
  { "foreign.dfam_acc" => "self.dfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2011-01-11 15:01:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:RJ11zV2Nel9BYh3uulj/7g


# You can replace this text with custom content, and it will be preserved on regeneration
1;
