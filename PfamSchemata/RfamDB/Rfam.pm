package RfamDB::Rfam;

use strict;
use warnings;

use base 'DBIx::Class';

__PACKAGE__->load_components("Core");
__PACKAGE__->table("rfam");
__PACKAGE__->add_columns(
  "auto_rfam",
  { data_type => "INT", default_value => undef, is_nullable => 0, size => 10 },
  "auto_wiki",
  { data_type => "INT", default_value => "", is_nullable => 0, size => 10 },
  "rfam_acc",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 7 },
  "rfam_id",
  { data_type => "VARCHAR", default_value => "", is_nullable => 0, size => 40 },
  "description",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 100,
  },
  "author",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "seed_source",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "alignment_method",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "gathering_cutoff",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "trusted_cutoff",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "noise_cutoff",
  { data_type => "DOUBLE", default_value => undef, is_nullable => 1, size => 64 },
  "comment",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "previous_id",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "cmbuild",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "cmcalibrate",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "cmsearch",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "num_seed",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
  "num_full",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
  "type",
  {
    data_type => "VARCHAR",
    default_value => undef,
    is_nullable => 1,
    size => 50,
  },
  "structure_source",
  {
    data_type => "TINYTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 255,
  },
  "number_of_states",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "number_of_nodes",
  {
    data_type => "MEDIUMINT",
    default_value => undef,
    is_nullable => 1,
    size => 8,
  },
  "number_of_species",
  { data_type => "BIGINT", default_value => undef, is_nullable => 1, size => 20 },
  "taxonomic_domain",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "taxonomic_root",
  {
    data_type => "MEDIUMTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 16777215,
  },
  "full_structure",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "reference_structure",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "reference_sequence",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
  "structure_annotations",
  {
    data_type => "LONGTEXT",
    default_value => undef,
    is_nullable => 1,
    size => 4294967295,
  },
);
__PACKAGE__->set_primary_key("auto_rfam");
__PACKAGE__->add_unique_constraint("rfam_acc", ["rfam_acc"]);
__PACKAGE__->has_many(
  "alignments_and_trees",
  "RfamDB::AlignmentsAndTrees",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "clan_memberships",
  "RfamDB::ClanMembership",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "html_alignments",
  "RfamDB::HtmlAlignments",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "pdb_rfam_regs",
  "RfamDB::PdbRfamReg",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->belongs_to("auto_wiki", "RfamDB::Wikitext", { auto_wiki => "auto_wiki" });
__PACKAGE__->has_many(
  "rfam_cms",
  "RfamDB::RfamCm",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "rfam_database_links",
  "RfamDB::RfamDatabaseLinks",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "rfam_literature_references",
  "RfamDB::RfamLiteratureReferences",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "rfam_reg_fulls",
  "RfamDB::RfamRegFull",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "rfam_reg_seeds",
  "RfamDB::RfamRegSeed",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);
__PACKAGE__->has_many(
  "secondary_structure_images",
  "RfamDB::SecondaryStructureImages",
  { "foreign.auto_rfam" => "self.auto_rfam" },
);


# Created by DBIx::Class::Schema::Loader v0.04004 @ 2010-01-12 10:09:30
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3ApIXss5kX4PoGzi5seYUg

__PACKAGE__->belongs_to(
  'article',
  'RfamDB::Wikitext',
  { 'foreign.auto_wiki' => 'self.auto_wiki' }
);

__PACKAGE__->belongs_to(
  'from_dead',
  'RfamDB::DeadFamilies',
  { 'foreign.forward_to' => 'self.rfam_acc' }
);

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk), 
         Jennifer Daub (jd7@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
