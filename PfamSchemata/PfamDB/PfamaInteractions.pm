use utf8;
package PfamDB::PfamaInteractions;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

PfamDB::PfamaInteractions

=cut

use strict;
use warnings;

use base 'DBIx::Class::Core';

=head1 TABLE: C<pfamA_interactions>

=cut

__PACKAGE__->table("pfamA_interactions");

=head1 ACCESSORS

=head2 pfama_acc_a

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=head2 pfama_acc_b

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 7

=cut

__PACKAGE__->add_columns(
  "pfama_acc_a",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
  "pfama_acc_b",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 7 },
);

=head1 RELATIONS

=head2 pfama_acc_a

Type: belongs_to

Related object: L<PfamDB::Pfama>

=cut

__PACKAGE__->belongs_to("pfama_acc_a", "PfamDB::Pfama", { pfama_acc => "pfama_acc_a" });

=head2 pfama_acc_b

Type: belongs_to

Related object: L<PfamDB::Pfama>

=cut

__PACKAGE__->belongs_to("pfama_acc_b", "PfamDB::Pfama", { pfama_acc => "pfama_acc_b" });


# Created by DBIx::Class::Schema::Loader v0.07042 @ 2015-04-22 10:42:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:NQsutgot/Do4wJ3BeGisFA


#__PACKAGE__->has_one( pfamA1 => 'PfamDB::Pfama',
#                      { 'foreign.auto_pfama'  => 'self.auto_pfama_a' },
#                      { proxy =>  [ qw( pfama_id pfama_acc ) ] } );

#__PACKAGE__->has_one( pfamA2 => 'PfamDB::Pfama',
#                      { 'foreign.auto_pfama'  => 'self.auto_pfama_b' },
#                      { proxy => [ qw( pfama_id pfama_acc ) ] } );

__PACKAGE__->might_have( clan_membership_a => 'PfamDB::ClanMembership',
                         { 'foreign.pfama_acc' => 'self.pfama_acc_a' } );

__PACKAGE__->might_have( clan_membership_b => 'PfamDB::ClanMembership',
                         { 'foreign.pfama_acc' => 'self.pfama_acc_b' } );

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
