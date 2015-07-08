use utf8;
package PfamDB::PfamaRegSeed;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::PfamaRegSeed

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_reg_seed>

=cut

__PACKAGE__->table("pfamA_reg_seed");

=head1 ACCESSORS

=head2 pfama_acc

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 pfamseq_acc

  data_type: 'varchar'
  is_nullable: 0
  size: 10

=head2 seq_start

  data_type: 'mediumint'
  default_value: 0
  is_nullable: 0

=head2 seq_end

  data_type: 'mediumint'
  is_nullable: 0

=head2 cigar

  data_type: 'text'
  is_nullable: 1

=head2 tree_order

  data_type: 'mediumint'
  is_nullable: 1

=head2 seq_version

  data_type: 'tinyint'
  is_nullable: 0

=head2 md5

  data_type: 'varchar'
  is_nullable: 0
  size: 32

=head2 source

  data_type: 'enum'
  extra: {list => ["pfamseq","uniprot"]}
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "pfama_acc",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "pfamseq_acc",
  { data_type => "varchar", is_nullable => 0, size => 10 },
  "seq_start",
  { data_type => "mediumint", default_value => 0, is_nullable => 0 },
  "seq_end",
  { data_type => "mediumint", is_nullable => 0 },
  "cigar",
  { data_type => "text", is_nullable => 1 },
  "tree_order",
  { data_type => "mediumint", is_nullable => 1 },
  "seq_version",
  { data_type => "tinyint", is_nullable => 0 },
  "md5",
  { data_type => "varchar", is_nullable => 0, size => 32 },
  "source",
  {
    data_type => "enum",
    extra => { list => ["pfamseq", "uniprot"] },
    is_nullable => 0,
  },
);

=head1 RELATIONS

=head2 pfama_acc

Type: belongs_to

Related object: L<PfamDB::Pfama>

=cut

__PACKAGE__->belongs_to("pfama_acc", "PfamDB::Pfama", { pfama_acc => "pfama_acc" });


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-07-08 11:37:26
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:TLPQ6J85FvJvX2kITFrG3Q

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("pfama_acc", "pfamseq_acc", "seq_start");

__PACKAGE__->has_one( "pfamseq" =>  "PfamDB::Pfamseq",
          { "foreign.pfamseq_acc"  => "self.pfamseq_acc" },
                    { proxy => [ qw ( pfamseq_acc pfamseq_id seq_version) ] } );

__PACKAGE__->has_one( "pfama" => "PfamDB::Pfama",
                    { "foreign.pfama_acc" => "self.pfama_acc"},
                      { proxy => [qw(pfama_id pfama_acc)]});

__PACKAGE__->might_have(
  "clan_membership" => 'PfamDB::ClanMembership',
			{ 'foreign.pfama_acc' => 'self.pfama_acc' } );



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
