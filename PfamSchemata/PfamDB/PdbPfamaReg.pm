use utf8;
package PfamDB::PdbPfamaReg;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::PdbPfamaReg

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pdb_pfamA_reg>

=cut

__PACKAGE__->table("pdb_pfamA_reg");

=head1 ACCESSORS

=head2 auto_pdb_reg

  data_type: 'integer'
  extra: {unsigned => 1}
  is_auto_increment: 1
  is_nullable: 0

=head2 auto_uniprot_reg_full

  data_type: 'integer'
  default_value: 0
  extra: {unsigned => 1}
  is_foreign_key: 1
  is_nullable: 0

=head2 pdb_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 5

=head2 pfama_acc

  data_type: 'varchar'
  default_value: 0
  is_nullable: 0
  size: 7

=head2 pfamseq_acc

  data_type: 'varchar'
  default_value: 0
  is_nullable: 0
  size: 10

=head2 chain

  data_type: 'varchar'
  is_nullable: 1
  size: 4

=head2 pdb_res_start

  data_type: 'mediumint'
  is_nullable: 1

=head2 pdb_start_icode

  data_type: 'varchar'
  is_nullable: 1
  size: 1

=head2 pdb_res_end

  data_type: 'mediumint'
  is_nullable: 1

=head2 pdb_end_icode

  data_type: 'varchar'
  is_nullable: 1
  size: 1

=head2 seq_start

  data_type: 'mediumint'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 seq_end

  data_type: 'mediumint'
  default_value: 0
  extra: {unsigned => 1}
  is_nullable: 0

=head2 hex_colour

  data_type: 'varchar'
  is_nullable: 1
  size: 6

=cut

__PACKAGE__->add_columns(
  "auto_pdb_reg",
  {
    data_type => "integer",
    extra => { unsigned => 1 },
    is_auto_increment => 1,
    is_nullable => 0,
  },
  "auto_uniprot_reg_full",
  {
    data_type => "integer",
    default_value => 0,
    extra => { unsigned => 1 },
    is_foreign_key => 1,
    is_nullable => 0,
  },
  "pdb_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 5 },
  "pfama_acc",
  { data_type => "varchar", default_value => 0, is_nullable => 0, size => 7 },
  "pfamseq_acc",
  { data_type => "varchar", default_value => 0, is_nullable => 0, size => 10 },
  "chain",
  { data_type => "varchar", is_nullable => 1, size => 4 },
  "pdb_res_start",
  { data_type => "mediumint", is_nullable => 1 },
  "pdb_start_icode",
  { data_type => "varchar", is_nullable => 1, size => 1 },
  "pdb_res_end",
  { data_type => "mediumint", is_nullable => 1 },
  "pdb_end_icode",
  { data_type => "varchar", is_nullable => 1, size => 1 },
  "seq_start",
  {
    data_type => "mediumint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "seq_end",
  {
    data_type => "mediumint",
    default_value => 0,
    extra => { unsigned => 1 },
    is_nullable => 0,
  },
  "hex_colour",
  { data_type => "varchar", is_nullable => 1, size => 6 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_pdb_reg>

=back

=cut

__PACKAGE__->set_primary_key("auto_pdb_reg");

=head1 RELATIONS

=head2 auto_uniprot_reg_full

Type: belongs_to

Related object: L<PfamDB::UniprotRegFull>

=cut

__PACKAGE__->belongs_to(
  "auto_uniprot_reg_full",
  "PfamDB::UniprotRegFull",
  { auto_uniprot_reg_full => "auto_uniprot_reg_full" },
);

=head2 pdb_id

Type: belongs_to

Related object: L<PfamDB::Pdb>

=cut

__PACKAGE__->belongs_to("pdb_id", "PfamDB::Pdb", { pdb_id => "pdb_id" });


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-09-22 11:23:35
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:3gX46IWVsBNpRl/T+iIUfA


__PACKAGE__->might_have(
  "pdb_image",
  "PfamDB::PdbImage",
  { "foreign.pdb_id" => "self.pdb_id" },
);

__PACKAGE__->belongs_to("pfamseq_acc", "PfamDB::Pfamseq", {pfamseq_acc => "pfamseq_acc"});
 __PACKAGE__->belongs_to("pfama_acc", "PfamDB::Pfama", {pfama_acc => "pfama_acc"});

__PACKAGE__->might_have(
  "clan_members",
  "PfamDB::ClanMembership",
  { "foreign.pfama_acc" => "self.pfama_acc" },
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
