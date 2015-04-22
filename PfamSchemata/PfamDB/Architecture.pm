use utf8;
package PfamDB::Architecture;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::Architecture

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<architecture>

=cut

__PACKAGE__->table("architecture");

=head1 ACCESSORS

=head2 auto_architecture

  data_type: 'bigint'
  extra: {unsigned => 1}
  is_nullable: 0

=head2 architecture

  data_type: 'text'
  is_nullable: 1

=head2 type_example

  data_type: 'varchar'
  default_value: 0
  is_nullable: 0
  size: 10

=head2 no_seqs

  data_type: 'integer'
  default_value: 0
  is_nullable: 0

=head2 architecture_acc

  data_type: 'text'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "auto_architecture",
  { data_type => "bigint", extra => { unsigned => 1 }, is_nullable => 0 },
  "architecture",
  { data_type => "text", is_nullable => 1 },
  "type_example",
  { data_type => "varchar", default_value => 0, is_nullable => 0, size => 10 },
  "no_seqs",
  { data_type => "integer", default_value => 0, is_nullable => 0 },
  "architecture_acc",
  { data_type => "text", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</auto_architecture>

=back

=cut

__PACKAGE__->set_primary_key("auto_architecture");

=head1 RELATIONS

=head2 clan_architectures

Type: has_many

Related object: L<PfamDB::ClanArchitecture>

=cut

__PACKAGE__->has_many(
  "clan_architectures",
  "PfamDB::ClanArchitecture",
  { "foreign.auto_architecture" => "self.auto_architecture" },
  undef,
);

=head2 pfama_architectures

Type: has_many

Related object: L<PfamDB::PfamaArchitecture>

=cut

__PACKAGE__->has_many(
  "pfama_architectures",
  "PfamDB::PfamaArchitecture",
  { "foreign.auto_architecture" => "self.auto_architecture" },
  undef,
);

=head2 proteome_architectures

Type: has_many

Related object: L<PfamDB::ProteomeArchitecture>

=cut

__PACKAGE__->has_many(
  "proteome_architectures",
  "PfamDB::ProteomeArchitecture",
  { "foreign.auto_architecture" => "self.auto_architecture" },
  undef,
);


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:UNJM7KbTC9Flfjkfrh4QQg


__PACKAGE__->has_one( 
  "storable",
  "PfamDB::PfamAnnseq",
  { "foreign.pfamseq_acc" => "self.type_example" },
  { proxy => [ qw/ annseq_storable / ] }
);

__PACKAGE__->has_one(
  "type_example",
  "PfamDB::Pfamseq",
  { "foreign.pfamseq_acc" => "self.type_example" },
  { proxy => [ qw/ pfamseq_id pfamseq_acc / ] }
);

__PACKAGE__->has_one(
  "clan_arch",
  "PfamDB::ClanArchitecture",
  { "foreign.auto_architecture" => "self.auto_architecture" },
  { proxy => [ qw/ auto_clan / ] }
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
