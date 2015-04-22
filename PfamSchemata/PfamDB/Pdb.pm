use utf8;
package PfamDB::Pdb;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::Pdb

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb>

=cut

__PACKAGE__->table("pdb");

=head1 ACCESSORS

=head2 pdb_id

  data_type: 'varchar'
  is_nullable: 0
  size: 5

=head2 keywords

  data_type: 'tinytext'
  is_nullable: 1

=head2 title

  data_type: 'mediumtext'
  is_nullable: 1

=head2 date

  data_type: 'tinytext'
  is_nullable: 1

=head2 resolution

  data_type: 'decimal'
  default_value: 0.00
  is_nullable: 1
  size: [5,2]

=head2 method

  data_type: 'tinytext'
  is_nullable: 1

=head2 author

  data_type: 'mediumtext'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "pdb_id",
  { data_type => "varchar", is_nullable => 0, size => 5 },
  "keywords",
  { data_type => "tinytext", is_nullable => 1 },
  "title",
  { data_type => "mediumtext", is_nullable => 1 },
  "date",
  { data_type => "tinytext", is_nullable => 1 },
  "resolution",
  {
    data_type => "decimal",
    default_value => "0.00",
    is_nullable => 1,
    size => [5, 2],
  },
  "method",
  { data_type => "tinytext", is_nullable => 1 },
  "author",
  { data_type => "mediumtext", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</pdb_id>

=back

=cut

__PACKAGE__->set_primary_key("pdb_id");

=head1 RELATIONS

=head2 pdb_images

Type: has_many

Related object: L<PfamDB::PdbImage>

=cut

__PACKAGE__->has_many(
  "pdb_images",
  "PfamDB::PdbImage",
  { "foreign.pdb_id" => "self.pdb_id" },
  undef,
);

=head2 pdb_pfama_regs

Type: has_many

Related object: L<PfamDB::PdbPfamaReg>

=cut

__PACKAGE__->has_many(
  "pdb_pfama_regs",
  "PfamDB::PdbPfamaReg",
  { "foreign.pdb_id" => "self.pdb_id" },
  undef,
);

=head2 pdb_residue_datas

Type: has_many

Related object: L<PfamDB::PdbResidueData>

=cut

__PACKAGE__->has_many(
  "pdb_residue_datas",
  "PfamDB::PdbResidueData",
  { "foreign.pdb_id" => "self.pdb_id" },
  undef,
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:sCgKSNq5luqGrTxaluFMnw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
1;
