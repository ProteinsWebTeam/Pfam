use utf8;
package PfamDB::Pfamseq;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::Pfamseq

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamseq>

=cut

__PACKAGE__->table("pfamseq");

=head1 ACCESSORS

=head2 pfamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=head2 pfamseq_id

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 seq_version

  data_type: 'tinyint'
  is_nullable: 0

=head2 crc64

  data_type: 'varchar'
  is_nullable: 0
  size: 16

=head2 md5

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=head2 description

  data_type: 'text'
  is_nullable: 0

=head2 evidence

  data_type: 'tinyint'
  is_foreign_key: 1
  is_nullable: 0

=head2 length

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 species

  data_type: 'text'
  is_nullable: 0

=head2 taxonomy

  data_type: 'mediumtext'
  is_nullable: 1

=head2 is_fragment

  data_type: 'tinyint'
  is_nullable: 1

=head2 sequence

  accessor: undef
  data_type: 'blob'
  is_nullable: 0

=head2 updated

  data_type: 'timestamp'
  datetime_undef_if_invalid: 1
  default_value: current_timestamp
  is_nullable: 0

=head2 created

  data_type: 'datetime'
  datetime_undef_if_invalid: 1
  is_nullable: 1

=head2 ncbi_taxid

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 auto_architecture

  data_type: 'bigint'
  extra: {unsigned => 1}
  is_nullable: 1

=head2 treefam_acc

  data_type: 'varchar'
  is_nullable: 1
  size: 8

=cut

__PACKAGE__->add_columns(
  "pfamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "pfamseq_id",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "seq_version",
  { data_type => "tinyint", is_nullable => 0 },
  "crc64",
  { data_type => "varchar", is_nullable => 0, size => 16 },
  "md5",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "description",
  { data_type => "text", is_nullable => 0 },
  "evidence",
  { data_type => "tinyint", is_foreign_key => 1, is_nullable => 0 },
  "length",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "species",
  { data_type => "text", is_nullable => 0 },
  "taxonomy",
  { data_type => "mediumtext", is_nullable => 1 },
  "is_fragment",
  { data_type => "tinyint", is_nullable => 1 },
  "sequence",
  { accessor => undef, data_type => "blob", is_nullable => 0 },
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
  "ncbi_taxid",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "auto_architecture",
  { data_type => "bigint", extra => { unsigned => 1 }, is_nullable => 1 },
  "treefam_acc",
  { data_type => "varchar", is_nullable => 1, size => 8 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pfamseq_acc>

=back

=cut

__PACKAGE__->set_primary_key("pfamseq_acc");

=head1 RELATIONS

=head2 edits

Type: has_many

Related object: L<PfamDB::Edits>

=cut

__PACKAGE__->has_many(
  "edits",
  "PfamDB::Edits",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
  undef,
);

=head2 evidence

Type: belongs_to

Related object: L<PfamDB::Evidence>

=cut

__PACKAGE__->belongs_to("evidence", "PfamDB::Evidence", { evidence => "evidence" });

=head2 ncbi_taxid

Type: belongs_to

Related object: L<PfamDB::NcbiTaxonomy>

=cut

__PACKAGE__->belongs_to(
  "ncbi_taxid",
  "PfamDB::NcbiTaxonomy",
  { ncbi_taxid => "ncbi_taxid" },
);

=head2 nested_locations

Type: has_many

Related object: L<PfamDB::NestedLocations>

=cut

__PACKAGE__->has_many(
  "nested_locations",
  "PfamDB::NestedLocations",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
  undef,
);

=head2 other_regs

Type: has_many

Related object: L<PfamDB::OtherReg>

=cut

__PACKAGE__->has_many(
  "other_regs",
  "PfamDB::OtherReg",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
  undef,
);

=head2 pfam_annseqs

Type: has_many

Related object: L<PfamDB::PfamAnnseq>

=cut

__PACKAGE__->has_many(
  "pfam_annseqs",
  "PfamDB::PfamAnnseq",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
  undef,
);

=head2 pfama_reg_full_insignificants

Type: has_many

Related object: L<PfamDB::PfamaRegFullInsignificant>

=cut

__PACKAGE__->has_many(
  "pfama_reg_full_insignificants",
  "PfamDB::PfamaRegFullInsignificant",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
  undef,
);

=head2 pfama_reg_full_significants

Type: has_many

Related object: L<PfamDB::PfamaRegFullSignificant>

=cut

__PACKAGE__->has_many(
  "pfama_reg_full_significants",
  "PfamDB::PfamaRegFullSignificant",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
  undef,
);

=head2 pfamseq_disulphides

Type: has_many

Related object: L<PfamDB::PfamseqDisulphide>

=cut

__PACKAGE__->has_many(
  "pfamseq_disulphides",
  "PfamDB::PfamseqDisulphide",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
  undef,
);

=head2 pfamseq_markups

Type: has_many

Related object: L<PfamDB::PfamseqMarkup>

=cut

__PACKAGE__->has_many(
  "pfamseq_markups",
  "PfamDB::PfamseqMarkup",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
  undef,
);

=head2 secondary_pfamseq_accs

Type: has_many

Related object: L<PfamDB::SecondaryPfamseqAcc>

=cut

__PACKAGE__->has_many(
  "secondary_pfamseq_accs",
  "PfamDB::SecondaryPfamseqAcc",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
  undef,
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2016-05-17 15:56:03
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:TgBRhAMleYdNgv5TQUj5wA


#__PACKAGE__->has_many(
#  "pfamseq_markups",
#  "PfamDB::PfamseqMarkup",
#  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
#);

__PACKAGE__->has_one(
  "annseqs",
  "PfamDB::PfamAnnseq",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
);

#__PACKAGE__->has_many(
#  "proteome_pfamseqs",
#  "PfamDB::ProteomePfamseq",
#  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
#);

# this relationship needs to have the same name as the one that links
# PfamaArchitecture to Architecture
__PACKAGE__->belongs_to(
  "auto_architecture",
  "PfamDB::Architecture",
  { "foreign.auto_architecture" => "self.auto_architecture" },
);


__PACKAGE__->add_columns(
  "sequence",
  { data_type => "blob", is_nullable => 0 },
);

__PACKAGE__->has_many(
  "pfama_reg_seeds",
  "PfamDB::PfamaRegSeed",
  { "foreign.pfamseq_acc" => "self.pfamseq_acc" },
);

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

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
