use utf8;
package PfamDB::Pfama;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::Pfama

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
  is_nullable: 0
  size: 7

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
  extra: {list => ["Family","Domain","Repeat","Motif","Disordered","Coiled-coil"]}
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

=head2 number_archs

  data_type: 'integer'
  is_nullable: 1

=head2 number_species

  data_type: 'integer'
  is_nullable: 1

=head2 number_structures

  data_type: 'integer'
  is_nullable: 1

=head2 number_ncbi

  data_type: 'integer'
  is_nullable: 1

=head2 number_meta

  data_type: 'integer'
  is_nullable: 1

=head2 average_length

  data_type: 'double precision'
  is_nullable: 1
  size: [6,2]

=head2 percentage_id

  data_type: 'integer'
  is_nullable: 1

=head2 average_coverage

  data_type: 'double precision'
  is_nullable: 1
  size: [6,2]

=head2 change_status

  data_type: 'tinytext'
  is_nullable: 1

=head2 seed_consensus

  data_type: 'text'
  is_nullable: 1

=head2 full_consensus

  data_type: 'text'
  is_nullable: 1

=head2 number_shuffled_hits

  data_type: 'integer'
  is_nullable: 1

=head2 number_uniprot

  data_type: 'integer'
  is_nullable: 1

=head2 rp_seed

  data_type: 'tinyint'
  is_nullable: 1

=head2 number_rp15

  data_type: 'integer'
  is_nullable: 1

=head2 number_rp35

  data_type: 'integer'
  is_nullable: 1

=head2 number_rp55

  data_type: 'integer'
  is_nullable: 1

=head2 number_rp75

  data_type: 'integer'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_nullable => 0, size => 7 },
  "pfama_id",
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
    extra => {
      list => [
        "Family",
        "Domain",
        "Repeat",
        "Motif",
        "Disordered",
        "Coiled-coil",
      ],
    },
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
  "number_archs",
  { data_type => "integer", is_nullable => 1 },
  "number_species",
  { data_type => "integer", is_nullable => 1 },
  "number_structures",
  { data_type => "integer", is_nullable => 1 },
  "number_ncbi",
  { data_type => "integer", is_nullable => 1 },
  "number_meta",
  { data_type => "integer", is_nullable => 1 },
  "average_length",
  { data_type => "double precision", is_nullable => 1, size => [6, 2] },
  "percentage_id",
  { data_type => "integer", is_nullable => 1 },
  "average_coverage",
  { data_type => "double precision", is_nullable => 1, size => [6, 2] },
  "change_status",
  { data_type => "tinytext", is_nullable => 1 },
  "seed_consensus",
  { data_type => "text", is_nullable => 1 },
  "full_consensus",
  { data_type => "text", is_nullable => 1 },
  "number_shuffled_hits",
  { data_type => "integer", is_nullable => 1 },
  "number_uniprot",
  { data_type => "integer", is_nullable => 1 },
  "rp_seed",
  { data_type => "tinyint", is_nullable => 1 },
  "number_rp15",
  { data_type => "integer", is_nullable => 1 },
  "number_rp35",
  { data_type => "integer", is_nullable => 1 },
  "number_rp55",
  { data_type => "integer", is_nullable => 1 },
  "number_rp75",
  { data_type => "integer", is_nullable => 1 },
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

=head2 _active_site_alignments

Type: has_many

Related object: L<PfamDB::ActiveSiteAlignments>

=cut

__PACKAGE__->has_many(
  "_active_site_alignments",
  "PfamDB::ActiveSiteAlignments",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 _pfama_internals

Type: has_many

Related object: L<PfamDB::PfamaInternal>

=cut

__PACKAGE__->has_many(
  "_pfama_internals",
  "PfamDB::PfamaInternal",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 alignment_and_trees

Type: has_many

Related object: L<PfamDB::AlignmentAndTree>

=cut

__PACKAGE__->has_many(
  "alignment_and_trees",
  "PfamDB::AlignmentAndTree",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 clan_memberships

Type: has_many

Related object: L<PfamDB::ClanMembership>

=cut

__PACKAGE__->has_many(
  "clan_memberships",
  "PfamDB::ClanMembership",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 current_pfam_versions

Type: has_many

Related object: L<PfamDB::CurrentPfamVersion>

=cut

__PACKAGE__->has_many(
  "current_pfam_versions",
  "PfamDB::CurrentPfamVersion",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 edits

Type: has_many

Related object: L<PfamDB::Edits>

=cut

__PACKAGE__->has_many(
  "edits",
  "PfamDB::Edits",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 gene_ontologies

Type: has_many

Related object: L<PfamDB::GeneOntology>

=cut

__PACKAGE__->has_many(
  "gene_ontologies",
  "PfamDB::GeneOntology",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 interpros

Type: has_many

Related object: L<PfamDB::Interpro>

=cut

__PACKAGE__->has_many(
  "interpros",
  "PfamDB::Interpro",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 nested_domains_nests_pfama_accs

Type: has_many

Related object: L<PfamDB::NestedDomains>

=cut

__PACKAGE__->has_many(
  "nested_domains_nests_pfama_accs",
  "PfamDB::NestedDomains",
  { "foreign.nests_pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 nested_domains_pfama_accs

Type: has_many

Related object: L<PfamDB::NestedDomains>

=cut

__PACKAGE__->has_many(
  "nested_domains_pfama_accs",
  "PfamDB::NestedDomains",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 nested_locations_nested_pfama_accs

Type: has_many

Related object: L<PfamDB::NestedLocations>

=cut

__PACKAGE__->has_many(
  "nested_locations_nested_pfama_accs",
  "PfamDB::NestedLocations",
  { "foreign.nested_pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 nested_locations_pfama_accs

Type: has_many

Related object: L<PfamDB::NestedLocations>

=cut

__PACKAGE__->has_many(
  "nested_locations_pfama_accs",
  "PfamDB::NestedLocations",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 pfama2pfama_hhsearch_pfama_acc_1s

Type: has_many

Related object: L<PfamDB::Pfama2pfamaHhsearch>

=cut

__PACKAGE__->has_many(
  "pfama2pfama_hhsearch_pfama_acc_1s",
  "PfamDB::Pfama2pfamaHhsearch",
  { "foreign.pfama_acc_1" => "self.pfama_acc" },
  undef,
);

=head2 pfama2pfama_hhsearch_pfama_acc_2s

Type: has_many

Related object: L<PfamDB::Pfama2pfamaHhsearch>

=cut

__PACKAGE__->has_many(
  "pfama2pfama_hhsearch_pfama_acc_2s",
  "PfamDB::Pfama2pfamaHhsearch",
  { "foreign.pfama_acc_2" => "self.pfama_acc" },
  undef,
);

=head2 pfama2pfama_scoop_pfama_acc_1s

Type: has_many

Related object: L<PfamDB::Pfama2pfamaScoop>

=cut

__PACKAGE__->has_many(
  "pfama2pfama_scoop_pfama_acc_1s",
  "PfamDB::Pfama2pfamaScoop",
  { "foreign.pfama_acc_1" => "self.pfama_acc" },
  undef,
);

=head2 pfama2pfama_scoop_pfama_acc_2s

Type: has_many

Related object: L<PfamDB::Pfama2pfamaScoop>

=cut

__PACKAGE__->has_many(
  "pfama2pfama_scoop_pfama_acc_2s",
  "PfamDB::Pfama2pfamaScoop",
  { "foreign.pfama_acc_2" => "self.pfama_acc" },
  undef,
);

=head2 pfama_architectures

Type: has_many

Related object: L<PfamDB::PfamaArchitecture>

=cut

__PACKAGE__->has_many(
  "pfama_architectures",
  "PfamDB::PfamaArchitecture",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 pfama_database_links

Type: has_many

Related object: L<PfamDB::PfamaDatabaseLinks>

=cut

__PACKAGE__->has_many(
  "pfama_database_links",
  "PfamDB::PfamaDatabaseLinks",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 pfama_fastas

Type: has_many

Related object: L<PfamDB::PfamaFasta>

=cut

__PACKAGE__->has_many(
  "pfama_fastas",
  "PfamDB::PfamaFasta",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 pfama_hmms

Type: has_many

Related object: L<PfamDB::PfamaHmm>

=cut

__PACKAGE__->has_many(
  "pfama_hmms",
  "PfamDB::PfamaHmm",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 pfama_interactions_pfama_acc_as

Type: has_many

Related object: L<PfamDB::PfamaInteractions>

=cut

__PACKAGE__->has_many(
  "pfama_interactions_pfama_acc_as",
  "PfamDB::PfamaInteractions",
  { "foreign.pfama_acc_a" => "self.pfama_acc" },
  undef,
);

=head2 pfama_interactions_pfama_acc_bs

Type: has_many

Related object: L<PfamDB::PfamaInteractions>

=cut

__PACKAGE__->has_many(
  "pfama_interactions_pfama_acc_bs",
  "PfamDB::PfamaInteractions",
  { "foreign.pfama_acc_b" => "self.pfama_acc" },
  undef,
);

=head2 pfama_literature_references

Type: has_many

Related object: L<PfamDB::PfamaLiteratureReference>

=cut

__PACKAGE__->has_many(
  "pfama_literature_references",
  "PfamDB::PfamaLiteratureReference",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 pfama_ncbis

Type: has_many

Related object: L<PfamDB::PfamaNcbi>

=cut

__PACKAGE__->has_many(
  "pfama_ncbis",
  "PfamDB::PfamaNcbi",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 pfama_reg_full_insignificants

Type: has_many

Related object: L<PfamDB::PfamaRegFullInsignificant>

=cut

__PACKAGE__->has_many(
  "pfama_reg_full_insignificants",
  "PfamDB::PfamaRegFullInsignificant",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 pfama_reg_full_significants

Type: has_many

Related object: L<PfamDB::PfamaRegFullSignificant>

=cut

__PACKAGE__->has_many(
  "pfama_reg_full_significants",
  "PfamDB::PfamaRegFullSignificant",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 pfama_reg_seeds

Type: has_many

Related object: L<PfamDB::PfamaRegSeed>

=cut

__PACKAGE__->has_many(
  "pfama_reg_seeds",
  "PfamDB::PfamaRegSeed",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 pfama_species_trees

Type: has_many

Related object: L<PfamDB::PfamaSpeciesTree>

=cut

__PACKAGE__->has_many(
  "pfama_species_trees",
  "PfamDB::PfamaSpeciesTree",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 pfama_tax_depths

Type: has_many

Related object: L<PfamDB::PfamaTaxDepth>

=cut

__PACKAGE__->has_many(
  "pfama_tax_depths",
  "PfamDB::PfamaTaxDepth",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 pfama_wikis

Type: has_many

Related object: L<PfamDB::PfamaWiki>

=cut

__PACKAGE__->has_many(
  "pfama_wikis",
  "PfamDB::PfamaWiki",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 proteome_regions

Type: has_many

Related object: L<PfamDB::ProteomeRegions>

=cut

__PACKAGE__->has_many(
  "proteome_regions",
  "PfamDB::ProteomeRegions",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 released_pfam_versions

Type: has_many

Related object: L<PfamDB::ReleasedPfamVersion>

=cut

__PACKAGE__->has_many(
  "released_pfam_versions",
  "PfamDB::ReleasedPfamVersion",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);

=head2 uniprot_reg_fulls

Type: has_many

Related object: L<PfamDB::UniprotRegFull>

=cut

__PACKAGE__->has_many(
  "uniprot_reg_fulls",
  "PfamDB::UniprotRegFull",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  undef,
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2016-05-17 15:56:03
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:qqBFb9XGri1/aAT3VC8DEA



__PACKAGE__->has_one(
  "pfama_species_trees2",
  "PfamDB::PfamaSpeciesTree",
  "pfama_acc");

# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
