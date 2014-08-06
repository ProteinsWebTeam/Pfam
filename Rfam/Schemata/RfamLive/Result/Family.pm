use utf8;
package RfamLive::Result::Family;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

RfamLive::Result::Family

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<family>

=cut

__PACKAGE__->table("family");

=head1 ACCESSORS

=head2 rfam_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 7

=head2 rfam_id

  data_type: 'varchar'
  is_nullable: 0
  size: 40

=head2 auto_wiki

  data_type: 'integer'
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

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

=head2 comment

  data_type: 'longtext'
  is_nullable: 1

=head2 previous_id

  data_type: 'tinytext'
  is_nullable: 1

=head2 cmbuild

  data_type: 'tinytext'
  is_nullable: 1

=head2 cmcalibrate

  data_type: 'tinytext'
  is_nullable: 1

=head2 cmsearch

  data_type: 'tinytext'
  is_nullable: 1

=head2 num_seed

  data_type: 'bigint'
  is_nullable: 1

=head2 num_full

  data_type: 'bigint'
  is_nullable: 1

=head2 num_genome_seq

  data_type: 'bigint'
  is_nullable: 1

=head2 num_refseq

  data_type: 'bigint'
  is_nullable: 1

=head2 type

  data_type: 'varchar'
  is_nullable: 1
  size: 50

=head2 structure_source

  data_type: 'tinytext'
  is_nullable: 1

=head2 number_of_species

  data_type: 'bigint'
  is_nullable: 1

=head2 number_3d_structures

  data_type: 'integer'
  is_nullable: 1

=head2 tax_seed

  data_type: 'mediumtext'
  is_nullable: 1

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
  "rfam_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "rfam_id",
  { data_type => "varchar", is_nullable => 0, size => 40 },
  "auto_wiki",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
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
  "comment",
  { data_type => "longtext", is_nullable => 1 },
  "previous_id",
  { data_type => "tinytext", is_nullable => 1 },
  "cmbuild",
  { data_type => "tinytext", is_nullable => 1 },
  "cmcalibrate",
  { data_type => "tinytext", is_nullable => 1 },
  "cmsearch",
  { data_type => "tinytext", is_nullable => 1 },
  "num_seed",
  { data_type => "bigint", is_nullable => 1 },
  "num_full",
  { data_type => "bigint", is_nullable => 1 },
  "num_genome_seq",
  { data_type => "bigint", is_nullable => 1 },
  "num_refseq",
  { data_type => "bigint", is_nullable => 1 },
  "type",
  { data_type => "varchar", is_nullable => 1, size => 50 },
  "structure_source",
  { data_type => "tinytext", is_nullable => 1 },
  "number_of_species",
  { data_type => "bigint", is_nullable => 1 },
  "number_3d_structures",
  { data_type => "integer", is_nullable => 1 },
  "tax_seed",
  { data_type => "mediumtext", is_nullable => 1 },
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

=item * L</rfam_acc>

=back

=cut

__PACKAGE__->set_primary_key("rfam_acc");

=head1 RELATIONS

=head2 alignments_and_tree

Type: has_many

Related object: L<RfamLive::Result::AlignmentAndTree>

=cut

__PACKAGE__->has_many(
  "alignments_and_tree",
  "RfamLive::Result::AlignmentAndTree",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 auto_wiki

Type: belongs_to

Related object: L<RfamLive::Result::Wikitext>

=cut

__PACKAGE__->belongs_to(
  "auto_wiki",
  "RfamLive::Result::Wikitext",
  { auto_wiki => "auto_wiki" },
  { is_deferrable => 1, on_delete => "NO ACTION", on_update => "NO ACTION" },
);

=head2 clan_membership

Type: might_have

Related object: L<RfamLive::Result::ClanMembership>

=cut

__PACKAGE__->might_have(
  "clan_membership",
  "RfamLive::Result::ClanMembership",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 database_links

Type: has_many

Related object: L<RfamLive::Result::DatabaseLink>

=cut

__PACKAGE__->has_many(
  "database_links",
  "RfamLive::Result::DatabaseLink",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 families_long

Type: has_many

Related object: L<RfamLive::Result::FamilyLong>

=cut

__PACKAGE__->has_many(
  "families_long",
  "RfamLive::Result::FamilyLong",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 family_files

Type: has_many

Related object: L<RfamLive::Result::FamilyFile>

=cut

__PACKAGE__->has_many(
  "family_files",
  "RfamLive::Result::FamilyFile",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 family_literature_references

Type: has_many

Related object: L<RfamLive::Result::FamilyLiteratureReference>

=cut

__PACKAGE__->has_many(
  "family_literature_references",
  "RfamLive::Result::FamilyLiteratureReference",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 family_ncbis

Type: has_many

Related object: L<RfamLive::Result::FamilyNcbi>

=cut

__PACKAGE__->has_many(
  "family_ncbis",
  "RfamLive::Result::FamilyNcbi",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 full_regions

Type: has_many

Related object: L<RfamLive::Result::FullRegion>

=cut

__PACKAGE__->has_many(
  "full_regions",
  "RfamLive::Result::FullRegion",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 genome_full_regions

Type: has_many

Related object: L<RfamLive::Result::GenomeFullRegion>

=cut

__PACKAGE__->has_many(
  "genome_full_regions",
  "RfamLive::Result::GenomeFullRegion",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 html_alignments

Type: has_many

Related object: L<RfamLive::Result::HtmlAlignment>

=cut

__PACKAGE__->has_many(
  "html_alignments",
  "RfamLive::Result::HtmlAlignment",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 matches_and_fasta

Type: has_many

Related object: L<RfamLive::Result::MatchAndFasta>

=cut

__PACKAGE__->has_many(
  "matches_and_fasta",
  "RfamLive::Result::MatchAndFasta",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 overlap_memberships

Type: has_many

Related object: L<RfamLive::Result::OverlapMembership>

=cut

__PACKAGE__->has_many(
  "overlap_memberships",
  "RfamLive::Result::OverlapMembership",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pdb_rfam_regs

Type: has_many

Related object: L<RfamLive::Result::PdbRfamReg>

=cut

__PACKAGE__->has_many(
  "pdb_rfam_regs",
  "RfamLive::Result::PdbRfamReg",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 post_processes

Type: has_many

Related object: L<RfamLive::Result::PostProcess>

=cut

__PACKAGE__->has_many(
  "post_processes",
  "RfamLive::Result::PostProcess",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 processed_datas

Type: has_many

Related object: L<RfamLive::Result::ProcessedData>

=cut

__PACKAGE__->has_many(
  "processed_datas",
  "RfamLive::Result::ProcessedData",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 refseq_full_regions

Type: has_many

Related object: L<RfamLive::Result::RefseqFullRegion>

=cut

__PACKAGE__->has_many(
  "refseq_full_regions",
  "RfamLive::Result::RefseqFullRegion",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 secondary_structure_images

Type: has_many

Related object: L<RfamLive::Result::SecondaryStructureImage>

=cut

__PACKAGE__->has_many(
  "secondary_structure_images",
  "RfamLive::Result::SecondaryStructureImage",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 seed_regions

Type: has_many

Related object: L<RfamLive::Result::SeedRegion>

=cut

__PACKAGE__->has_many(
  "seed_regions",
  "RfamLive::Result::SeedRegion",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 sunbursts

Type: has_many

Related object: L<RfamLive::Result::Sunburst>

=cut

__PACKAGE__->has_many(
  "sunbursts",
  "RfamLive::Result::Sunburst",
  { "foreign.rfam_acc" => "self.rfam_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07039 @ 2014-06-02 13:07:10
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:UrIoPFlsjSho8XhSCvBmeA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
