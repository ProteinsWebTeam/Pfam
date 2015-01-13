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

=head2 number_ref_proteome

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
  "number_rp15",
  { data_type => "integer", is_nullable => 1 },
  "number_rp35",
  { data_type => "integer", is_nullable => 1 },
  "number_rp55",
  { data_type => "integer", is_nullable => 1 },
  "number_rp75",
  { data_type => "integer", is_nullable => 1 },
  "number_ref_proteome",
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

=head2 active_site_alignments

Type: has_many

Related object: L<PfamLive::Result::ActiveSiteAlignment>

=cut

__PACKAGE__->has_many(
  "active_site_alignments",
  "PfamLive::Result::ActiveSiteAlignment",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 alignments_and_tree

Type: has_many

Related object: L<PfamLive::Result::AlignmentAndTree>

=cut

__PACKAGE__->has_many(
  "alignments_and_tree",
  "PfamLive::Result::AlignmentAndTree",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 clan_memberships

Type: has_many

Related object: L<PfamLive::Result::ClanMembership>

=cut

__PACKAGE__->has_many(
  "clan_memberships",
  "PfamLive::Result::ClanMembership",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 current_pfam_versions

Type: has_many

Related object: L<PfamLive::Result::CurrentPfamVersion>

=cut

__PACKAGE__->has_many(
  "current_pfam_versions",
  "PfamLive::Result::CurrentPfamVersion",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 edits

Type: has_many

Related object: L<PfamLive::Result::Edit>

=cut

__PACKAGE__->has_many(
  "edits",
  "PfamLive::Result::Edit",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 gene_ontologies

Type: has_many

Related object: L<PfamLive::Result::GeneOntology>

=cut

__PACKAGE__->has_many(
  "gene_ontologies",
  "PfamLive::Result::GeneOntology",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 interpros

Type: has_many

Related object: L<PfamLive::Result::Interpro>

=cut

__PACKAGE__->has_many(
  "interpros",
  "PfamLive::Result::Interpro",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 nested_domains_nests_pfama_accs

Type: has_many

Related object: L<PfamLive::Result::NestedDomain>

=cut

__PACKAGE__->has_many(
  "nested_domains_nests_pfama_accs",
  "PfamLive::Result::NestedDomain",
  { "foreign.nests_pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 nested_domains_pfama_accs

Type: has_many

Related object: L<PfamLive::Result::NestedDomain>

=cut

__PACKAGE__->has_many(
  "nested_domains_pfama_accs",
  "PfamLive::Result::NestedDomain",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 nested_locations_nested_pfama_accs

Type: has_many

Related object: L<PfamLive::Result::NestedLocation>

=cut

__PACKAGE__->has_many(
  "nested_locations_nested_pfama_accs",
  "PfamLive::Result::NestedLocation",
  { "foreign.nested_pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 nested_locations_pfama_accs

Type: has_many

Related object: L<PfamLive::Result::NestedLocation>

=cut

__PACKAGE__->has_many(
  "nested_locations_pfama_accs",
  "PfamLive::Result::NestedLocation",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a2pfam_a_hhsearch_pfama_acc_1s

Type: has_many

Related object: L<PfamLive::Result::PfamA2pfamAHhsearch>

=cut

__PACKAGE__->has_many(
  "pfam_a2pfam_a_hhsearch_pfama_acc_1s",
  "PfamLive::Result::PfamA2pfamAHhsearch",
  { "foreign.pfama_acc_1" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a2pfam_a_hhsearch_pfama_acc_2s

Type: has_many

Related object: L<PfamLive::Result::PfamA2pfamAHhsearch>

=cut

__PACKAGE__->has_many(
  "pfam_a2pfam_a_hhsearch_pfama_acc_2s",
  "PfamLive::Result::PfamA2pfamAHhsearch",
  { "foreign.pfama_acc_2" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a2pfam_a_scoop_pfama_acc_1s

Type: has_many

Related object: L<PfamLive::Result::PfamA2pfamAScoop>

=cut

__PACKAGE__->has_many(
  "pfam_a2pfam_a_scoop_pfama_acc_1s",
  "PfamLive::Result::PfamA2pfamAScoop",
  { "foreign.pfama_acc_1" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a2pfam_a_scoop_pfama_acc_2s

Type: has_many

Related object: L<PfamLive::Result::PfamA2pfamAScoop>

=cut

__PACKAGE__->has_many(
  "pfam_a2pfam_a_scoop_pfama_acc_2s",
  "PfamLive::Result::PfamA2pfamAScoop",
  { "foreign.pfama_acc_2" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_architectures

Type: has_many

Related object: L<PfamLive::Result::PfamAArchitecture>

=cut

__PACKAGE__->has_many(
  "pfam_a_architectures",
  "PfamLive::Result::PfamAArchitecture",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_database_links

Type: has_many

Related object: L<PfamLive::Result::PfamADatabaseLink>

=cut

__PACKAGE__->has_many(
  "pfam_a_database_links",
  "PfamLive::Result::PfamADatabaseLink",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_fastas

Type: has_many

Related object: L<PfamLive::Result::PfamAFasta>

=cut

__PACKAGE__->has_many(
  "pfam_a_fastas",
  "PfamLive::Result::PfamAFasta",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

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

=head2 pfam_a_interactions_pfama_acc_bs

Type: has_many

Related object: L<PfamLive::Result::PfamAInteraction>

=cut

__PACKAGE__->has_many(
  "pfam_a_interactions_pfama_acc_bs",
  "PfamLive::Result::PfamAInteraction",
  { "foreign.pfama_acc_b" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_interactions_pfama_accs_a

Type: has_many

Related object: L<PfamLive::Result::PfamAInteraction>

=cut

__PACKAGE__->has_many(
  "pfam_a_interactions_pfama_accs_a",
  "PfamLive::Result::PfamAInteraction",
  { "foreign.pfama_acc_a" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_internals

Type: has_many

Related object: L<PfamLive::Result::PfamAInternal>

=cut

__PACKAGE__->has_many(
  "pfam_a_internals",
  "PfamLive::Result::PfamAInternal",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_ligands

Type: has_many

Related object: L<PfamLive::Result::PfamALigand>

=cut

__PACKAGE__->has_many(
  "pfam_a_ligands",
  "PfamLive::Result::PfamALigand",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_literature_references

Type: has_many

Related object: L<PfamLive::Result::PfamALiteratureReference>

=cut

__PACKAGE__->has_many(
  "pfam_a_literature_references",
  "PfamLive::Result::PfamALiteratureReference",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_ncbis

Type: has_many

Related object: L<PfamLive::Result::PfamANcbi>

=cut

__PACKAGE__->has_many(
  "pfam_a_ncbis",
  "PfamLive::Result::PfamANcbi",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_reg_full_insignificants

Type: has_many

Related object: L<PfamLive::Result::PfamARegFullInsignificant>

=cut

__PACKAGE__->has_many(
  "pfam_a_reg_full_insignificants",
  "PfamLive::Result::PfamARegFullInsignificant",
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

=head2 pfam_a_species_trees

Type: has_many

Related object: L<PfamLive::Result::PfamASpeciesTree>

=cut

__PACKAGE__->has_many(
  "pfam_a_species_trees",
  "PfamLive::Result::PfamASpeciesTree",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_tax_depths

Type: has_many

Related object: L<PfamLive::Result::PfamATaxDepth>

=cut

__PACKAGE__->has_many(
  "pfam_a_tax_depths",
  "PfamLive::Result::PfamATaxDepth",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_wikis

Type: has_many

Related object: L<PfamLive::Result::PfamAWiki>

=cut

__PACKAGE__->has_many(
  "pfam_a_wikis",
  "PfamLive::Result::PfamAWiki",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 proteome_regions

Type: has_many

Related object: L<PfamLive::Result::ProteomeRegion>

=cut

__PACKAGE__->has_many(
  "proteome_regions",
  "PfamLive::Result::ProteomeRegion",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 released_pfam_versions

Type: has_many

Related object: L<PfamLive::Result::ReleasedPfamVersion>

=cut

__PACKAGE__->has_many(
  "released_pfam_versions",
  "PfamLive::Result::ReleasedPfamVersion",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 ligands

Type: many_to_many

Composing rels: L</pfam_a_ligands> -> ligand

=cut

__PACKAGE__->many_to_many("ligands", "pfam_a_ligands", "ligand");


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-01-13 08:53:22
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:UJF3iHMFBfEjCKPEpD0i1Q
# These lines were loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PfamA.pm' found in @INC.
# They are now part of the custom portion of this file
# for you to hand-edit.  If you do not either delete
# this section or remove that file from @INC, this section
# will be repeated redundantly when you re-create this
# file again via Loader!  See skip_load_external to disable
# this feature.

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

=head2 number_ref_proteome

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
  "number_rp15",
  { data_type => "integer", is_nullable => 1 },
  "number_rp35",
  { data_type => "integer", is_nullable => 1 },
  "number_rp55",
  { data_type => "integer", is_nullable => 1 },
  "number_rp75",
  { data_type => "integer", is_nullable => 1 },
  "number_ref_proteome",
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

=head2 active_site_alignments

Type: has_many

Related object: L<PfamLive::Result::ActiveSiteAlignment>

=cut

__PACKAGE__->has_many(
  "active_site_alignments",
  "PfamLive::Result::ActiveSiteAlignment",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 alignments_and_tree

Type: has_many

Related object: L<PfamLive::Result::AlignmentAndTree>

=cut

__PACKAGE__->has_many(
  "alignments_and_tree",
  "PfamLive::Result::AlignmentAndTree",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 clan_memberships

Type: has_many

Related object: L<PfamLive::Result::ClanMembership>

=cut

__PACKAGE__->has_many(
  "clan_memberships",
  "PfamLive::Result::ClanMembership",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 current_pfam_versions

Type: has_many

Related object: L<PfamLive::Result::CurrentPfamVersion>

=cut

__PACKAGE__->has_many(
  "current_pfam_versions",
  "PfamLive::Result::CurrentPfamVersion",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 edits

Type: has_many

Related object: L<PfamLive::Result::Edit>

=cut

__PACKAGE__->has_many(
  "edits",
  "PfamLive::Result::Edit",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 gene_ontologies

Type: has_many

Related object: L<PfamLive::Result::GeneOntology>

=cut

__PACKAGE__->has_many(
  "gene_ontologies",
  "PfamLive::Result::GeneOntology",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 interpros

Type: has_many

Related object: L<PfamLive::Result::Interpro>

=cut

__PACKAGE__->has_many(
  "interpros",
  "PfamLive::Result::Interpro",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 nested_domains_nests_pfama_accs

Type: has_many

Related object: L<PfamLive::Result::NestedDomain>

=cut

__PACKAGE__->has_many(
  "nested_domains_nests_pfama_accs",
  "PfamLive::Result::NestedDomain",
  { "foreign.nests_pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 nested_domains_pfama_accs

Type: has_many

Related object: L<PfamLive::Result::NestedDomain>

=cut

__PACKAGE__->has_many(
  "nested_domains_pfama_accs",
  "PfamLive::Result::NestedDomain",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 nested_locations_nested_pfama_accs

Type: has_many

Related object: L<PfamLive::Result::NestedLocation>

=cut

__PACKAGE__->has_many(
  "nested_locations_nested_pfama_accs",
  "PfamLive::Result::NestedLocation",
  { "foreign.nested_pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 nested_locations_pfama_accs

Type: has_many

Related object: L<PfamLive::Result::NestedLocation>

=cut

__PACKAGE__->has_many(
  "nested_locations_pfama_accs",
  "PfamLive::Result::NestedLocation",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a2pfam_a_hhsearch_pfama_acc_1s

Type: has_many

Related object: L<PfamLive::Result::PfamA2pfamAHhsearch>

=cut

__PACKAGE__->has_many(
  "pfam_a2pfam_a_hhsearch_pfama_acc_1s",
  "PfamLive::Result::PfamA2pfamAHhsearch",
  { "foreign.pfama_acc_1" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a2pfam_a_hhsearch_pfama_acc_2s

Type: has_many

Related object: L<PfamLive::Result::PfamA2pfamAHhsearch>

=cut

__PACKAGE__->has_many(
  "pfam_a2pfam_a_hhsearch_pfama_acc_2s",
  "PfamLive::Result::PfamA2pfamAHhsearch",
  { "foreign.pfama_acc_2" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a2pfam_a_scoop_pfama_acc_1s

Type: has_many

Related object: L<PfamLive::Result::PfamA2pfamAScoop>

=cut

__PACKAGE__->has_many(
  "pfam_a2pfam_a_scoop_pfama_acc_1s",
  "PfamLive::Result::PfamA2pfamAScoop",
  { "foreign.pfama_acc_1" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a2pfam_a_scoop_pfama_acc_2s

Type: has_many

Related object: L<PfamLive::Result::PfamA2pfamAScoop>

=cut

__PACKAGE__->has_many(
  "pfam_a2pfam_a_scoop_pfama_acc_2s",
  "PfamLive::Result::PfamA2pfamAScoop",
  { "foreign.pfama_acc_2" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_architectures

Type: has_many

Related object: L<PfamLive::Result::PfamAArchitecture>

=cut

__PACKAGE__->has_many(
  "pfam_a_architectures",
  "PfamLive::Result::PfamAArchitecture",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_database_links

Type: has_many

Related object: L<PfamLive::Result::PfamADatabaseLink>

=cut

__PACKAGE__->has_many(
  "pfam_a_database_links",
  "PfamLive::Result::PfamADatabaseLink",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_fastas

Type: has_many

Related object: L<PfamLive::Result::PfamAFasta>

=cut

__PACKAGE__->has_many(
  "pfam_a_fastas",
  "PfamLive::Result::PfamAFasta",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

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

=head2 pfam_a_interactions_pfama_acc_bs

Type: has_many

Related object: L<PfamLive::Result::PfamAInteraction>

=cut

__PACKAGE__->has_many(
  "pfam_a_interactions_pfama_acc_bs",
  "PfamLive::Result::PfamAInteraction",
  { "foreign.pfama_acc_b" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_interactions_pfama_accs_a

Type: has_many

Related object: L<PfamLive::Result::PfamAInteraction>

=cut

__PACKAGE__->has_many(
  "pfam_a_interactions_pfama_accs_a",
  "PfamLive::Result::PfamAInteraction",
  { "foreign.pfama_acc_a" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_internals

Type: has_many

Related object: L<PfamLive::Result::PfamAInternal>

=cut

__PACKAGE__->has_many(
  "pfam_a_internals",
  "PfamLive::Result::PfamAInternal",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_ligands

Type: has_many

Related object: L<PfamLive::Result::PfamALigand>

=cut

__PACKAGE__->has_many(
  "pfam_a_ligands",
  "PfamLive::Result::PfamALigand",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_literature_references

Type: has_many

Related object: L<PfamLive::Result::PfamALiteratureReference>

=cut

__PACKAGE__->has_many(
  "pfam_a_literature_references",
  "PfamLive::Result::PfamALiteratureReference",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_ncbis

Type: has_many

Related object: L<PfamLive::Result::PfamANcbi>

=cut

__PACKAGE__->has_many(
  "pfam_a_ncbis",
  "PfamLive::Result::PfamANcbi",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_reg_full_insignificants

Type: has_many

Related object: L<PfamLive::Result::PfamARegFullInsignificant>

=cut

__PACKAGE__->has_many(
  "pfam_a_reg_full_insignificants",
  "PfamLive::Result::PfamARegFullInsignificant",
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

=head2 pfam_a_species_trees

Type: has_many

Related object: L<PfamLive::Result::PfamASpeciesTree>

=cut

__PACKAGE__->has_many(
  "pfam_a_species_trees",
  "PfamLive::Result::PfamASpeciesTree",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_tax_depths

Type: has_many

Related object: L<PfamLive::Result::PfamATaxDepth>

=cut

__PACKAGE__->has_many(
  "pfam_a_tax_depths",
  "PfamLive::Result::PfamATaxDepth",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 pfam_a_wikis

Type: has_many

Related object: L<PfamLive::Result::PfamAWiki>

=cut

__PACKAGE__->has_many(
  "pfam_a_wikis",
  "PfamLive::Result::PfamAWiki",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 proteome_regions

Type: has_many

Related object: L<PfamLive::Result::ProteomeRegion>

=cut

__PACKAGE__->has_many(
  "proteome_regions",
  "PfamLive::Result::ProteomeRegion",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 released_pfam_versions

Type: has_many

Related object: L<PfamLive::Result::ReleasedPfamVersion>

=cut

__PACKAGE__->has_many(
  "released_pfam_versions",
  "PfamLive::Result::ReleasedPfamVersion",
  { "foreign.pfama_acc" => "self.pfama_acc" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 ligands

Type: many_to_many

Composing rels: L</pfam_a_ligands> -> ligand

=cut

__PACKAGE__->many_to_many("ligands", "pfam_a_ligands", "ligand");


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2014-12-05 15:05:07
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Kru3gvYaVShR+VXpPz1MjQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
# End of lines loaded from '/nfs/production/xfam/pfam/software/Modules/PfamSchemata/PfamLive/Result/PfamA.pm' 


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
